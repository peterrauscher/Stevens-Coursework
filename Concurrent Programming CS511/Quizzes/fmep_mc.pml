byte turn = 0;
bool flag[2];

proctype T(byte me) {
    do
    ::  do
        :: turn != me ->
            do
            ::  flag[1-me]
            ::  else -> break;
            od;
            turn = me;
        :: else -> break;
        od;
        printf("%x went in\n", me);
        assert(flag[me] != flag[1-me]);
        progress: flag[me] = false;
        printf("%x went out\n", me);
    od;
}

init {
    int x;
    for(x: 0..1) {
        flag[x] = false;
    }
    int i;
    atomic {
        for(i: 0..1) {
            run T(i);
        }
    }
}