package de.thiesgerken.moneydb.util;

import android.util.Log;

import io.swagger.client.ApiCallback;

/**
 * Created by thies on 07.05.17.
 */

public abstract class NoStatusApiCallback<T> implements ApiCallback<T> {
    private final static String TAG = "ApiCallback";
    private boolean notified = false;

    @Override
    public void onUploadProgress(long l, long l1, boolean b) {
        if (b && !notified) {
            Log.v(TAG, "Uploaded " + l + " Bytes");
            notified = true;
        }
    }

    @Override
    public void onDownloadProgress(long l, long l1, boolean b) {
        if (b && !notified) {
            Log.v(TAG, "Downloaded " + l + " Bytes");
            notified = true;
        }
    }
}
