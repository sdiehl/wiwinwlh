/* $(CC) -c qsort.c -o qsort.o */
void swap(int *a, int *b)
{
    int t = *a;
    *a = *b;
    *b = t;
}

void sort(int *xs, int beg, int end)
{
    if (end > beg + 1) {
        int piv = xs[beg], l = beg + 1, r = end;

        while (l < r) {
            if (xs[l] <= piv) {
                l++;
            } else {
                swap(&xs[l], &xs[--r]);
            }
        }

        swap(&xs[--l], &xs[beg]);
        sort(xs, beg, l);
        sort(xs, r, end);
    }
}
