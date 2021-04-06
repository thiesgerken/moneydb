package de.thiesgerken.moneydb.util;

/**
 * Created by thies on 13.04.17.
 */

public abstract class RunnableArg<T> implements Runnable {
    private T arg;

    public void run(T v) {
        setArg(v);
        run();
    }

    public T getArg() {
        return arg;
    }

    public void setArg(T v) {
        arg = v;
    }
}
