package de.thiesgerken.moneydb.notifications;

import android.util.Log;

import com.google.firebase.iid.FirebaseInstanceId;
import com.google.firebase.iid.FirebaseInstanceIdService;

/**
 * Created by thies on 30.04.17.
 */

public class MyInstanceIdService
        extends FirebaseInstanceIdService {

    private final String TAG = "MyInstanceIdService";

    @Override
    public void onTokenRefresh() {
        // Get updated InstanceID token.
        String refreshedToken = FirebaseInstanceId.getInstance().getToken();
        Log.d(TAG, "Refreshed token: " + refreshedToken);

        (new UpdateHandler(this)).refreshToken(refreshedToken);
    }

}
