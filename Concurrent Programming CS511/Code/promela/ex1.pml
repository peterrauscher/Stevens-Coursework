
int x;

active proctype P() {
    do
        ::  if
                :: x < 200 -> x++;
            fi;
            assert(x >= 0 && x <= 200);
    od;
}

active proctype Q() {
    do
        ::  if
                :: x > 0 -> x--;
            fi;
            assert(x >= 0 && x <= 200);
    od;
}

active proctype R() {
    do
        ::  if
                :: x == 200 -> x = 0;
            fi;
            assert(x >= 0 && x <= 200);
    od;
}

init {
    run P();
    run Q();
    run R();
}