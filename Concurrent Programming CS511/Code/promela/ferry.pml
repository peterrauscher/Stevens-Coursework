byte CAPACITY = 20;
byte permissionToGetOn[2];
byte permissionToSetSail = CAPACITY;
byte permissionToGetOff = 0;
byte permissionToReboard = 0;

inline acquire(sem) {
    atomic {
        sem > 0;
        sem--;
    }
}

inline release(sem) {
    sem++;
}

active proctype Ferry () {
    byte coast = 0;
    int i;
    do
        ::
            for (i: 1..CAPACITY) {
                release(permissionToGetOn[coast]);
            };
            for (i: 1..CAPACITY) {
                acquire(permissionToSetSail);
            };
            assert(permissionToSetSail == 0);
            /* move to other coast */
            coast = (coast+1) % 2;
            /* reached other coast */
            for (i: 1..CAPACITY) {
                release(permissionToGetOff);
            }
            for (i: 1..CAPACITY) {
                acquire(permissionToReboard);
            }
    od
}

proctype PassengerAtCoast ( byte coast ) {
    acquire ( permissionToGetOn [ coast ]);
    release ( permissionToSetSail );
    /* waiting to arrive at other coast */
    acquire ( permissionToGetOff );
    
    release ( permissionToReboard );
}
