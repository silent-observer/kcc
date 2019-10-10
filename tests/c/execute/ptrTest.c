int main() {
    int x0; int x1; int x2; int x3; int x4; int x5;
    int *p = &x5;
    int i = 2;
    int j = 1;
    int y = 0;

    *p = 10;
    *(p + 1) = 20;
    *(p + i) = 30;
    *(p + (i + j)) = 40;
    *((p + j) + (i + j)) = 50;

    y += *p;
    y += *(p + 1);
    y += *(p + i);
    y += *(p + (i + j));
    y += *((p + j) + (i + j));
    return y;
}