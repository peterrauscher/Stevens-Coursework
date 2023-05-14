import React from 'react';

export default function usePersistedState(key, defaultValue) {
    const [state, setState] = React.useState(() => {
        const persistedState = localStorage.getItem(key);
        return persistedState ? persistedState : defaultValue;
    });
    React.useEffect(() => {
        window.localStorage.setItem(key, state);
    }, [state, key]);
    return [state, setState];
}
