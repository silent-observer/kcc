when not declared(Generator):
  include common

proc getOtherRegPointer(r: Register): Register {.inline.} =
  if r == Register(1): Register(2) else: Register(1)

proc test0Pointer(g: var Generator, r: Register) {.inline, used.} =
  g.output &= &"  CMP {r}\p"
proc testTwoPointer(g: var Generator, r1, r2: Register) {.inline, used.} =
  g.output &= &"  CMP {r1}, {r2}\p"
proc testConstPointer(g: var Generator, r1: Register, num: int64) {.inline, used.} =
  g.output &= &"  CMP {r1}, {num}\p"
proc ltConditionPointer(): string {.inline, used.} = "C"
proc geConditionPointer(): string {.inline, used.} = "NC"

proc loadConstPointer(g: var Generator, r: Register, num: int64) {.inline, used.} =
  g.output &= &"  LOAD {r}, {num}\p"

proc pushOnStackPointer(g: var Generator, r: Register) {.inline, used.} =
  g.output &= &"  SW (SP), {r}\p" &
              &"  SUBI SP, 4\p"

proc popFromStackPointer(g: var Generator, r: Register) {.inline, used.} =
  g.output &= &"  ADDI SP, 4\p" &
              &"  LW {r}, (SP)\p"

proc writeToStackPointer(offset: int, g: var Generator, dataReg: Register) {.used.} =
  g.output &= &"  SW (FP{offset:+}), {dataReg}\p"

proc writeToVarPointer(ast: ResolvedVarNode, g: var Generator, offset: int, dataReg: Register) {.used.} =
  if ast.isGlobal:
    let otherReg = dataReg.getOtherRegPointer()
    if offset != 0:
      g.output &= &"  LOAD {otherReg}, {ast.varName}[{offset}]\p"
    else:
      g.output &= &"  LOAD {otherReg}, {ast.varName}\p"
    g.output &= &"  SW ({otherReg}), {dataReg}\p"
  else:
    writeToStackPointer(ast.offset + offset, g, dataReg)

proc readFromVarPointer(ast: ResolvedVarNode, g: var Generator, offset: int, target: Register) {.used.} =
  if ast.isGlobal:
    if offset != 0:
      g.output &= &"  LOAD {target}, {ast.varName}[{offset}]\p"
    else:
      g.output &= &"  LOAD {target}, {ast.varName}\p"
    g.output &= &"  LW {target}, ({target})\p"
  else:
    g.output &= &"  LW {target}, (FP{ast.offset+offset:+})\p"

proc writeToAddrPointer(address: Address, g: var Generator, dataReg: Register) {.used.} =
  let offset = address.offset
  case address.kind:
    of Label:
      let otherReg = dataReg.getOtherRegPointer()
      if offset != 0:
        g.output &= &"  LOAD {otherReg}, {address.label}[{offset}]\p"
      else:
        g.output &= &"  LOAD {otherReg}, {address.label}\p"
      g.output &= &"  SW ({otherReg}), {dataReg}\p"
    of RelativeFP:
      g.output &= &"  SW (FP{offset + address.fpOffset:+}), {dataReg}\p"
    of Expression:
      let otherReg = dataReg.getOtherRegPointer()
      address.exp.generate(g, otherReg)
      g.output &= &"  SW ({otherReg}{offset:+}), {dataReg}\p"

proc readFromAddrPointer(address: Address, g: var Generator, target: Register) {.used.} =
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

proc writeToAddrInRegPointer(g: var Generator, dataReg, addrReg: Register) {.used.} =
  g.output &= &"  SW ({addrReg}), {dataReg}\p"

proc moveRegsPointer(g: var Generator, dest, src: Register) {.inline, used.} =
  g.output &= &"  MOV {dest}, {src}\p"

proc generatePointer(ast: UnaryExprNode, g: var Generator, target: Register) {.used.} =
  if ast.exp of DereferenceExprNode and ast.operator in ["++", "--"]:
    let otherReg = target.getOtherRegPointer()
    ast.exp.DereferenceExprNode.exp.generate(g, otherReg)
    g.output &= &"  LW {target}, ({otherReg})\p"
    if ast.operator == "++":
      g.output &= &"  ADDI {target}, {ast.exp.typeData.getSize}\p"
    else:
      g.output &= &"  SUBI {target}, {ast.exp.typeData.getSize}\p"
    g.writeToAddrInRegPointer(target, otherReg)
    return
  ast.exp.generate(g, target)
  case ast.operator:
    of "++":
      
      if ast.exp of VarNode:
        ast.raiseError("Unresolved increment!")
      elif not (ast.exp of ResolvedVarNode):
        ast.raiseError("Cannot increment rvalue!")
      g.output &= &"  ADDI {target}, {ast.exp.typeData.getSize}\p"
      ast.exp.ResolvedVarNode.writeToVarPointer(g, 0, target)
    of "--":
      if ast.exp of VarNode:
        ast.raiseError("Unresolved decrement!")
      elif not (ast.exp of ResolvedVarNode):
        ast.raiseError("Cannot decrement rvalue!")
      g.output &= &"  SUBI {target}, {ast.exp.typeData.getSize}\p"
      ast.exp.ResolvedVarNode.writeToVarPointer(g, 0, target)
    else: assert(false)

proc generatePointer(ast: PostfixExprNode, g: var Generator, target: Register) =
  let otherReg = target.getOtherRegPointer()
  if ast.exp of DereferenceExprNode:
    ast.exp.DereferenceExprNode.exp.generate(g, otherReg)
    g.output &= &"  LW {target}, ({otherReg})\p"
    if ast.operator == "++":
      g.output &= &"  ADDI {target}, {ast.exp.typeData.getSize}\p"
    else:
      g.output &= &"  SUBI {target}, {ast.exp.typeData.getSize}\p"
    g.writeToAddrInRegPointer(target, otherReg)
    if ast.operator == "++":
      g.output &= &"  SUBI {target}, {ast.exp.typeData.getSize}\p"
    else:
      g.output &= &"  ADDI {target}, {ast.exp.typeData.getSize}\p"
    return
  
  if not (ast.exp of ResolvedVarNode) :
    ast.raiseError(
      if ast.operator == "++": "Unresolved increment!" 
      else: "Unresolved decrement!")
  ast.exp.generate(g, target)
  case ast.operator:
    of "++": g.output &= &"  ADDI {otherReg}, {target}, 1\p"
    of "--": g.output &= &"  SUBI {otherReg}, {target}, 1\p"
    else: assert(false)
  ast.exp.ResolvedVarNode.writeToVarPointer(g, 0, otherReg)


proc generatePointer(ast: BinaryExprNode, g: var Generator, target: Register) =
  let otherReg = target.getOtherRegPointer()
  let size = ast.typeData.ptrType[].getSize
  if ast.operator == "+" or ast.operator == "-":
    let ptrExpr = if ast.exp1.typeData.kind == PointerType: ast.exp1 else: ast.exp2
    let intExpr = if ast.exp1.typeData.kind == PointerType: ast.exp2 else: ast.exp1
    if g.shouldOptimize and ptrExpr.isOneLevel():
      intExpr.generate(g, target)
      g.multiplyByConst(target, size)
      ptrExpr.generate(g, otherReg)
    elif g.shouldOptimize and isOneLevelConstMult(size) and intExpr.isOneLevel():
      ptrExpr.generate(g, otherReg)
      intExpr.generate(g, target)
      g.multiplyByConst(target, size)
    else:
      intExpr.generate(g, target)
      g.multiplyByConst(target, size)
      g.output &= &"  SW (SP), {target}\p" &
                  &"  SUBI SP, 4\p"
      ptrExpr.generate(g, otherReg)
    
      g.output &= &"  ADDI SP, 4\p" &
                  &"  LW {target}, (SP)\p"
    # Operands: otherReg <op> target
    if ast.operator == "+":
      g.output &= &"  ADD {target}, {otherReg}, {target}\p"
    else:
      g.output &= &"  SUB {target}, {otherReg}, {target}\p"
    return
  else:
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
        &"  LDI?C* {target}, 1\p" &
        &"  LDI?NC* {target}, 0\p"
    of ">=": g.output &= 
        &"  CMP {otherReg}, {target}\p" &
        &"  LDI?C* {target}, 0\p" &
        &"  LDI?NC* {target}, 1\p"
    of ">": g.output &= 
        &"  CMP {target}, {otherReg}\p" &
        &"  LDI?C* {target}, 1\p" &
        &"  LDI?NC* {target}, 0\p"
    of "<=": g.output &= 
        &"  CMP {target}, {otherReg}\p" &
        &"  LDI?C* {target}, 0\p" &
        &"  LDI?NC* {target}, 1\p"
    else: assert(false)

proc generatePointer(ast: BinaryRightConstExprNode, g: var Generator, target: Register) =
  let size = ast.typeData.ptrType[].getSize
  ast.exp1.generate(g, target)
  # Operands: target <op> num
  case ast.operator:
    of "-": g.output &= &"  SUBI {target}, {ast.num * size}\p"
    of "+": g.output &= &"  ADDI {target}, {ast.num * size}\p"
    else: assert(false)

proc generatePointer(ast: DereferenceExprNode, g: var Generator, target: Register) {.used.} =
  ast.exp.generate(g, target)
  g.output &= &"  LW {target}, ({target})\p"

proc generatePointer(ast: ConstNumberNode, g: var Generator, target: Register) =
  g.output &= &"  LOAD {target}, {ast.num}\p"

proc generatePointer(ast: ConvertExprNode, g: var Generator, target: Register) =
  ast.exp.generate(g, target)
  let t = ast.exp.typeData
  case t.kind:
    of UnknownType: assert(false, "Convert from unknown type!")
    of FunctionType: assert(false, "Convert from function type!")
    of StructType: assert(false, "Convert from struct type!")
    of PointerType: discard
    of ArrayType, ArrayOfUnknownSizeType: discard
    of SimpleType:
      case t.simpleType:
        of Int32, UInt32: discard
        of Int8, Int16, UInt8, UInt16: ast.raiseError(&"Attempt to cast {t} to {ast.typeData}!")