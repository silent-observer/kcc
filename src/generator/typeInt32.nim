when not declared(Generator):
  include common

proc getOtherRegInt32(r: Register): Register {.inline.} =
  if r == Register(1): Register(2) else: Register(1)

proc test0Int32(g: var Generator, r: Register) {.inline.} =
  g.output &= &"  CMP {r}\p"
proc testTwoInt32(g: var Generator, r1, r2: Register) {.inline.} =
  g.output &= &"  CMP {r1}, {r2}\p"
proc testConstInt32(g: var Generator, r1: Register, num: int64) {.inline.} =
  g.output &= &"  CMP {r1}, {num}\p"
proc ltConditionInt32(): string {.inline.} = "LT"
proc geConditionInt32(): string {.inline.} = "GE"

proc loadConstInt32(g: var Generator, r: Register, num: int64) {.inline.} =
  g.output &= &"  LOAD {r}, {num}\p"

proc pushOnStackInt32(g: var Generator, r: Register) {.inline.} =
  g.output &= &"  SW (SP), {r}\p" &
              &"  SUBI SP, 4\p"

proc popFromStackInt32(g: var Generator, r: Register) {.inline.} =
  g.output &= &"  ADDI SP, 4\p" &
              &"  LW {r}, (SP)\p"

proc writeToAddrInt32(address: Address, g: var Generator, dataReg: Register) =
  let offset = address.offset
  case address.kind:
    of Label:
      let otherReg = dataReg.getOtherRegInt32()
      if offset != 0:
        g.output &= &"  LOAD {otherReg}, {address.label}[{offset}]\p"
      else:
        g.output &= &"  LOAD {otherReg}, {address.label}\p"
      g.output &= &"  SW ({otherReg}), {dataReg}\p"
    of RelativeFP:
      g.output &= &"  SW (FP{offset + address.fpOffset:+}), {dataReg}\p"
    of Expression:
      let otherReg = dataReg.getOtherRegInt32()
      address.exp.generate(g, otherReg)
      g.output &= &"  SW ({otherReg}{offset:+}), {dataReg}\p"

proc readFromAddrInt32(address: Address, g: var Generator, target: Register) =
  let offset = address.offset
  case address.kind:
    of Label:
      if offset != 0:
        g.output &= &"  LOAD {target}, {address.label}[{offset}]\p"
      else:
        g.output &= &"  LOAD {target}, {address.label}\p"
      g.output &= &"  LW {target}, ({target})\p"
    of RelativeFP:
      g.output &= &"  LW {target}, (FP{offset + address.fpOffset:+})\p"
    of Expression:
      address.exp.generate(g, target)
      g.output &= &"  LW {target}, ({target}{offset:+})\p"

proc writeToAddrInRegInt32(g: var Generator, dataReg, addrReg: Register) =
  g.output &= &"  SW ({addrReg}), {dataReg}\p"

proc moveRegsInt32(g: var Generator, dest, src: Register) {.inline.} =
  g.output &= &"  MOV {dest}, {src}\p"

proc generateInt32(ast: UnaryExprNode, g: var Generator, target: Register) =
  if ast.exp of DereferenceExprNode and ast.operator in ["++", "--"]:
    let otherReg = target.getOtherRegInt32()
    ast.exp.DereferenceExprNode.exp.generate(g, otherReg)
    g.output &= &"  LW {target}, ({otherReg})\p"
    if ast.operator == "++":
      g.output &= &"  ADDI {target}, 1\p"
    else:
      g.output &= &"  SUBI {target}, 1\p"
    g.writeToAddrInRegInt32(target, otherReg)
    return
  ast.exp.generate(g, target)
  case ast.operator:
    of "-": g.output &= &"  SUB {target}, R0, {target}\p"
    of "~": g.output &= &"  XORI {target}, -1\p"
    of "!": g.output &= 
        &"  CMP {target}\p" &
        &"  LDI?Z* {target}, 1\p" &
        &"  LDI?NZ* {target}, 0\p"
    of "++":
      if ast.exp of VarNode:
        ast.raiseError("Unresolved increment!")
      elif not (ast.exp of ResolvedVarNode):
        ast.raiseError("Cannot increment rvalue!")
      g.output &= &"  ADDI {target}, 1\p"
      let address = ast.exp.getAddress(g)
      address.writeToAddrInt32(g, target)
    of "--":
      if ast.exp of VarNode:
        ast.raiseError("Unresolved decrement!")
      elif not (ast.exp of ResolvedVarNode):
        ast.raiseError("Cannot decrement rvalue!")
      g.output &= &"  ADDI {target}, 1\p"
      let address = ast.exp.getAddress(g)
      address.writeToAddrInt32(g, target)
    else: assert(false)

proc generateInt32(ast: PostfixExprNode, g: var Generator, target: Register) =
  let otherReg = target.getOtherRegInt32()
  if ast.exp of DereferenceExprNode:
    ast.exp.DereferenceExprNode.exp.generate(g, otherReg)
    g.output &= &"  LW {target}, ({otherReg})\p"
    if ast.operator == "++":
      g.output &= &"  ADDI {target}, 1\p"
    else:
      g.output &= &"  SUBI {target}, 1\p"
    g.writeToAddrInRegInt32(target, otherReg)
    if ast.operator == "++":
      g.output &= &"  SUBI {target}, 1\p"
    else:
      g.output &= &"  ADDI {target}, 1\p"
    return
  
  if not (ast.exp of ResolvedVarNode):
    ast.raiseError(
      if ast.operator == "++": "Unresolved increment!" 
      else: "Unresolved decrement!")
  
  ast.exp.generate(g, target)
  case ast.operator:
    of "++": g.output &= &"  ADDI {otherReg}, {target}, 1\p"
    of "--": g.output &= &"  SUBI {otherReg}, {target}, 1\p"
    else: assert(false)
  let address = ast.exp.getAddress(g)
  address.writeToAddrInt32(g, otherReg)

proc generateLogicalOrInt32(ast: BinaryExprNode, g: var Generator, target: Register) =
  let endLabel = g.generateLabel("logicOr")
  ast.exp1.jumpIf(g, endLabel, "")
  ast.exp2.generate(g, target)
  g.output &= 
    &"  CMP {target}\p" &
    &"{endLabel}:\p" &
    &"  LDI?NZ* {target}, 1\p"

proc generateLogicalAndInt32(ast: BinaryExprNode, g: var Generator, target: Register) =
  let endLabel = g.generateLabel("logicAnd")
  ast.exp1.jumpIf(g, "", endLabel)
  ast.exp2.generate(g, target)
  g.output &= 
    &"  CMP {target}\p" &
    &"  LDI?NZ* {target}, 1\p" &
    &"{endLabel}:\p" &
    &"  LDI?Z* {target}, 0\p"


proc generateInt32(ast: BinaryExprNode, g: var Generator, target: Register) =
  if (ast.operator == "||"):
    ast.generateLogicalOrInt32(g, target)
    return
  elif (ast.operator == "&&"):
    ast.generateLogicalAndInt32(g, target)
    return
  
  let otherReg = target.getOtherRegInt32()
  if g.shouldOptimize and ast.exp2.isOneLevel():
    ast.exp1.generate(g, otherReg)
    ast.exp2.generate(g, target)
  elif g.shouldOptimize and ast.exp1.isOneLevel():
    ast.exp2.generate(g, target)
    ast.exp1.generate(g, otherReg)
  else:
    ast.exp1.generate(g, target)
    g.output &= &"  SW (SP), {target}\p" &
                &"  SUBI SP, 4\p"
    ast.exp2.generate(g, target)
  
    g.output &= &"  ADDI SP, 4\p" &
                &"  LW {otherReg}, (SP)\p"
  # Operands: otherReg <op> target
  case ast.operator:
    of "-": g.output &= &"  SUB {target}, {otherReg}, {target}\p"
    of "+": g.output &= &"  ADD {target}, {otherReg}, {target}\p"
    of "*": g.output &= 
        &"  MLTS {otherReg}, {target}\p" &
        &"  MOV {target}, LO\p"
    of "/": g.output &= 
        &"  DIVS {otherReg}, {target}\p" &
         "  NOP\p".repeat(11) &
        &"  MOV {target}, LO\p"
    of "%": g.output &= 
        &"  DIVS {otherReg}, {target}\p" &
         "  NOP\p".repeat(11) &
        &"  MOV {target}, HI\p"
    of "==": g.output &= 
        &"  CMP {target}, {otherReg}\p" &
        &"  LDI?Z* {target}, 1\p" &
        &"  LDI?NZ* {target}, 0\p"
    of "!=": g.output &= 
        &"  CMP {target}, {otherReg}\p" &
        &"  LDI?Z* {target}, 0\p" &
        &"  LDI?NZ* {target}, 1\p"
    of "<": g.output &= 
        &"  CMP {otherReg}, {target}\p" &
        &"  LDI?LT* {target}, 1\p" &
        &"  LDI?GE* {target}, 0\p"
    of ">=": g.output &= 
        &"  CMP {otherReg}, {target}\p" &
        &"  LDI?LT* {target}, 0\p" &
        &"  LDI?GE* {target}, 1\p"
    of ">": g.output &= 
        &"  CMP {target}, {otherReg}\p" &
        &"  LDI?LT* {target}, 1\p" &
        &"  LDI?GE* {target}, 0\p"
    of "<=": g.output &= 
        &"  CMP {target}, {otherReg}\p" &
        &"  LDI?LT* {target}, 0\p" &
        &"  LDI?GE* {target}, 1\p"
    of "<<": g.output &= &"  ASH {target}, {otherReg}, {target}\p"
    of ">>": g.output &=
      &"  SUB {target}, R0, {target}\p" &
      &"  ASH {target}, {otherReg}, {target}\p"
    of "&": g.output &= &"  AND {target}, {otherReg}, {target}\p"
    of "^": g.output &= &"  XOR {target}, {otherReg}, {target}\p"
    of "|": g.output &= &"  OR {target}, {otherReg}, {target}\p"
    else: assert(false)

proc generateInt32(ast: BinaryRightConstExprNode, g: var Generator, target: Register) =
  let otherReg = target.getOtherRegInt32()
  ast.exp1.generate(g, target)
  # Operands: target <op> num
  case ast.operator:
    of "||": 
      if ast.num == 0:
        g.output &= 
          &"  CMP {target}\p" &
          &"  LDI?NZ {target}, 1\p"
      else:
        g.output &= &"  LDI {target}, 1\p"
    of "&&": 
      if ast.num != 0:
        g.output &= 
          &"  CMP {target}\p" &
          &"  LDI?NZ {target}, 1\p"
      else:
        g.output &= &"  LDI {target}, 0\p"

    of "-": g.output &= &"  SUBI {target}, {ast.num}\p"
    of "+": g.output &= &"  ADDI {target}, {ast.num}\p"
    of "*": g.multiplyByConst(target, ast.num.int)
    of "/": g.output &= 
        &"  LOAD {otherReg}, {ast.num}\p" &
        &"  DIVS {target}, {otherReg}\p" &
         "  NOP\p".repeat(11) &
        &"  MOV {target}, LO\p"
    of "%": g.output &= 
        &"  LOAD {otherReg}, {ast.num}\p" &
        &"  DIVS {target}, {otherReg}\p" &
         "  NOP\p".repeat(11) &
        &"  MOV {target}, HI\p"
    of "==": 
      g.output &= 
        &"  CMP {target}, {ast.num}\p" &
        &"  LDI?Z* {target}, 1\p" &
        &"  LDI?NZ* {target}, 0\p"
    of "!=": g.output &= 
        &"  CMP {target}, {ast.num}\p" &
        &"  LDI?Z* {target}, 0\p" &
        &"  LDI?NZ* {target}, 1\p"
    of "<": g.output &= 
        &"  CMP {target}, {ast.num}\p" &
        &"  LDI?LT* {target}, 1\p" &
        &"  LDI?GE* {target}, 0\p"
    of ">=": g.output &= 
        &"  CMP {target}, {ast.num}\p" &
        &"  LDI?LT* {target}, 0\p" &
        &"  LDI?GE* {target}, 1\p"
    of ">": g.output &= 
        &"  LOAD {otherReg}, {ast.num}\p" &
        &"  CMP {otherReg}, {target}\p" &
        &"  LDI?LT* {target}, 1\p" &
        &"  LDI?GE* {target}, 0\p"
    of "<=": g.output &= 
        &"  LOAD {otherReg}, {ast.num}\p" &
        &"  CMP {otherReg}, {target}\p" &
        &"  LDI?LT* {target}, 0\p" &
        &"  LDI?GE* {target}, 1\p"
    of "<<": g.output &= &"  ASHI {target}, {ast.num}\p"
    of ">>": g.output &= &"  ASHI {target}, {-ast.num}\p"
    of "&": g.output &= &"  ANDI {target}, {ast.num}\p"
    of "^": g.output &= &"  XORI {target}, {ast.num}\p"
    of "|": g.output &= &"  ORI {target}, {ast.num}\p"
    else: assert(false)

proc generateInt32(ast: ConvertExprNode, g: var Generator, target: Register) =
  ast.exp.generate(g, target)
  let t = ast.exp.typeData
  case t.kind:
    of UnknownType: assert(false, "Convert from unknown type!")
    of FunctionType: assert(false, "Convert from function type!")
    of StructType: assert(false, "Convert from struct type!")
    of ArrayType, ArrayOfUnknownSizeType: assert(false, "Convert from array type!")
    of PointerType: discard
    of SimpleType:
      case t.simpleType:
        of Int8, Int16, Int32,
           UInt8, UInt16, UInt32: discard