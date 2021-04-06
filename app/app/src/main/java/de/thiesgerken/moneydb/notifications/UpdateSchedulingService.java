package de.thiesgerken.moneydb.notifications;

import android.app.IntentService;
import android.content.Intent;


/**
 * This {@code IntentService} does the app's actual work.
 * {@code UpdateAlarmReceiver} (a {@code WakefulBroadcastReceiver}) holds a
 * partial wake lock for this service while the service does its work. When the
 * service is finished, it calls {@code completeWakefulIntent()} to release the
 * wake lock.
 */
public class UpdateSchedulingService extends IntentService {

    private UpdateHandler handler;

    public UpdateSchedulingService() {
        super("MoneyDB Expense Updates");
    }

    @Override
    public void onCreate() {
        super.onCreate();
        handler = new UpdateHandler(this);
    }

    @Override
    protected void onHandleIntent(final Intent intent) {
        handler.pullUpdates(ExpenseNotificationBuilder.ExpenseSource.PULL_PERIODIC, new Runnable() {
            @Override
            public void run() {
                UpdateAlarmReceiver.completeWakefulIntent(intent);
            }
        });
    }
}
