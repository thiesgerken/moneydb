package de.thiesgerken.moneydb.notifications;

import android.content.Context;
import android.content.SharedPreferences;
import android.preference.PreferenceManager;
import android.util.Log;

import com.jaredrummler.android.device.DeviceName;

import org.joda.time.DateTime;
import org.joda.time.format.ISODateTimeFormat;

import java.util.List;
import java.util.Map;

import de.thiesgerken.moneydb.Api;
import de.thiesgerken.moneydb.util.NoStatusApiCallback;
import io.swagger.client.ApiException;
import io.swagger.client.model.ExpenseFilter;
import io.swagger.client.model.RecordRenderedExpense;
import io.swagger.client.model.RenderedDevice;
import io.swagger.client.model.RenderedExpense;
import io.swagger.client.model.UpdateSettings;

/**
 * Created by thies on 01.05.17.
 */

public class UpdateHandler {

    private final String TAG = "UpdateHandler";
    SharedPreferences sharedPrefs;
    private Context context;
    private ExpenseNotificationBuilder notificationBuilder;

    public UpdateHandler(Context context) {
        this.context = context;
        this.notificationBuilder = ExpenseNotificationBuilder.newInstance(context);
        this.sharedPrefs = PreferenceManager.getDefaultSharedPreferences(context);
    }

    // TODO: read filters from settings
    private UpdateSettings getUpdateSettings() {
        String lastUpdate = sharedPrefs.getString("last_update", null);

        if (lastUpdate == null)
            lastUpdate = ISODateTimeFormat.dateTimeNoMillis().print(new DateTime());

        UpdateSettings settings = new UpdateSettings();
        settings.setSince(lastUpdate);

        settings.addFiltersItem(new ExpenseFilter()
                .onlySomeoneElse(true)
                .onlyNew(false)
                .onlyThrough(null).deviceId(-1L));
        settings.addFiltersItem(new ExpenseFilter()
                .onlySomeoneElse(false)
                .onlyNew(true)
                .onlyThrough("moneydb-client|moneydb-server").deviceId(-1L));

        return settings;
    }

    public void registerClient() {
        Log.d(TAG, "Registering client");

        RenderedDevice dev = new RenderedDevice();
        dev.setToken(sharedPrefs.getString("token", null));
        dev.setFirstContact(ISODateTimeFormat.dateTimeNoMillis().print(new DateTime()));
        dev.setLastContact(ISODateTimeFormat.dateTimeNoMillis().print(new DateTime()));
        dev.setNotificationCount(0);
        dev.setLastNotification(ISODateTimeFormat.dateTimeNoMillis().print(new DateTime()));
        dev.setFilters(getUpdateSettings().getFilters());
        dev.setModel(DeviceName.getDeviceName());

        try {
            Api.getInstance(context).apiDevicesRenderedPostAsync(dev, new NoStatusApiCallback<Long>() {
                @Override
                public void onFailure(ApiException e, int i, Map<String, List<String>> map) {
                    Log.e(TAG, "Failed to register device", e);
                    Log.e(TAG, e.getResponseBody());
                }

                @Override
                public void onSuccess(Long id, int i, Map<String, List<String>> map) {
                    Log.d(TAG, "Registered device");
                }
            });
        } catch (Exception ex) {
            Log.d(TAG, ex.toString());
        }
    }

    public void refreshToken(String token) {
        SharedPreferences.Editor editor = sharedPrefs.edit();
        editor.putString("token", token);
        editor.commit();

        registerClient();
    }

    public void pullExpense(final long id, final Runnable callback, final ExpenseNotificationBuilder.ExpenseSource source) {
        Log.d(TAG, "Pulling single expense " + id);

        Api.maybeRefresh(context, new Runnable() {
            @Override
            public void run() {
                try {
                    Api.getInstance(context).apiExpensesRenderedIdGetAsync((int) id, new NoStatusApiCallback<RenderedExpense>() {
                        @Override
                        public void onFailure(ApiException e, int i, Map<String, List<String>> map) {
                            Log.e(TAG, "Failed to pull expense " + id, e);
                        }

                        @Override
                        public void onSuccess(RenderedExpense expense, int i, Map<String, List<String>> map) {
                            RecordRenderedExpense record = new RecordRenderedExpense();
                            record.id(id).data(expense);
                            ExpenseNotificationBuilder.newInstance(context).sendNotification(record, source);
                            Log.d(TAG, "Pulled Expense " + id + ", title: '" + expense.getTitle() + "'");
                        }
                    });
                } catch (Exception ex) {
                    Log.d(TAG, "Error pulling expense");
                    Log.d(TAG, ex.toString());
                } finally {
                    if (callback != null)
                        callback.run();
                }
            }
        }, callback);
    }

    public void pullUpdates(final ExpenseNotificationBuilder.ExpenseSource source, final Runnable callback) {
        Log.d(TAG, "Pulling updates");

        Api.maybeRefresh(context, new Runnable() {
            @Override
            public void run() {
                try {
                    List<RecordRenderedExpense> updates = Api.getInstance(context). apiExpensesRenderedUpdatesPost(getUpdateSettings());

                    Log.d(TAG, updates.size() + " new expenses");

                    for (RecordRenderedExpense e : updates)
                        notificationBuilder.sendNotification(e, source);

                    String lastUpdate = ISODateTimeFormat.dateTimeNoMillis().print(new DateTime());
                    SharedPreferences.Editor editor = sharedPrefs.edit();
                    editor.putString("last_update", lastUpdate);
                    editor.commit();

                } catch (Exception ex) {
                    Log.d(TAG, "Error pulling updates");
                    Log.d(TAG, ex.toString());
                } finally {
                    if (callback != null)
                        callback.run();
                }
            }
        }, callback);
    }

}
