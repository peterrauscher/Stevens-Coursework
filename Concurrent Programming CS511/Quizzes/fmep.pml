byte turn = 0;
bool flag[2] = {false, false};

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
        flag[me] = false
        printf("%x went out\n", me);
    od;
}

init {
    int i;
    for(i: 0..1) {
        run T(i);
    }
}