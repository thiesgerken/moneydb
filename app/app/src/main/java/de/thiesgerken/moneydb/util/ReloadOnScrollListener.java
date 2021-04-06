package de.thiesgerken.moneydb.util;

import android.util.Log;
import android.widget.AbsListView;

/**
 * Created by thies on 02.05.17.
 */

public class ReloadOnScrollListener
        implements AbsListView.OnScrollListener {

    private final int THRESHOLD = 5;
    private final static String TAG = "ReloadOnScrollListener";

    private int previousTotal = 0;
    private boolean loading = true;
    private Runnable updater;

    public ReloadOnScrollListener(Runnable updater) {
        this.updater = updater;
    }

    @Override
    public void onScroll(AbsListView view, int firstVisibleItem,
                         int visibleItemCount, int totalItemCount) {
        if (loading) {
            if (totalItemCount != previousTotal && totalItemCount != 0) {
                loading = false;
                previousTotal = totalItemCount;
            }
        } else if (totalItemCount != 1 && (totalItemCount - (firstVisibleItem + visibleItemCount)) <= THRESHOLD) {
            loading = true;

            Log.d(TAG, "Loading more expenses (firstVisible = " + firstVisibleItem + ", visibleCount = " + visibleItemCount + ", totalCount = " + totalItemCount + ")");
            updater.run();
        }
    }

    @Override
    public void onScrollStateChanged(AbsListView view, int scrollState) {
    }
}
