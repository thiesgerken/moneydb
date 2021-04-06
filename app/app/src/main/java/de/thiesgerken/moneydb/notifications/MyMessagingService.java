package de.thiesgerken.moneydb.notifications;

import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.Context;
import android.content.Intent;
import android.media.RingtoneManager;
import android.net.Uri;
import android.support.v4.app.NotificationCompat;
import android.util.Log;

import com.google.firebase.messaging.FirebaseMessagingService;
import com.google.firebase.messaging.RemoteMessage;
import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;

import java.lang.reflect.Type;
import java.util.List;

import de.thiesgerken.moneydb.MainActivity;
import de.thiesgerken.moneydb.R;

public class MyMessagingService extends FirebaseMessagingService {
    private final String TAG = "MyMessagingService";

    @Override
    public void onMessageReceived(RemoteMessage remoteMessage) {
        Log.d(TAG, "from: " + remoteMessage.getFrom() + " data: " + remoteMessage.getData());
        // sendNotification("FCM Message " + remoteMessage.getData().toString());

        Context appContext = this.getApplicationContext();

        if (remoteMessage.getData().size() > 0) {
            UpdateHandler x = new UpdateHandler(appContext);

            try {
                Type listType = new TypeToken<List<Long>>() {
                }.getType();
                List<Long> ids = new Gson().fromJson(remoteMessage.getData().get("ids"), listType);
                Log.d(TAG, "payload: " + ids.toString());

                for (Long i : ids) {
                    x.pullExpense(i, null, ExpenseNotificationBuilder.ExpenseSource.PUSH_SINGLE);
                }
            } catch (Exception ex) {
                Log.e(TAG, "could not parse payload as list of longs", ex);
            }
        } else {
            Log.d(TAG, "no payload");

            (new UpdateHandler(appContext)).pullUpdates(ExpenseNotificationBuilder.ExpenseSource.PUSH_UPDATES, null);
        }

        if (remoteMessage.getNotification() != null) {
            Log.d(TAG, "Message Notification Body: " + remoteMessage.getNotification().getBody());
        }
    }
}
