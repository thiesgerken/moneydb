package de.thiesgerken.moneydb.notifications;

import android.app.RemoteInput;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.util.Log;

import com.google.gson.Gson;

import java.util.List;
import java.util.Map;

import de.thiesgerken.moneydb.Api;
import de.thiesgerken.moneydb.util.NoStatusApiCallback;
import io.swagger.client.ApiException;
import io.swagger.client.model.ExpenseFlagging;
import io.swagger.client.model.RecordRenderedExpense;
import io.swagger.client.model.RenderedExpense;

/**
 * Created by thies on 06.05.17.
 */

public class ExpenseReceiver extends BroadcastReceiver {
    private final String TAG = "ExpenseReceiver";

    private CharSequence getCommentText(Intent intent) {
        Bundle remoteInput = RemoteInput.getResultsFromIntent(intent);
        if (remoteInput != null)
            return remoteInput.getCharSequence(ExpenseNotificationBuilder.KEY_TEXT_COMMENT);

        return null;
    }

    @Override
    public void onReceive(final Context context, Intent intent) {
        final Gson gson = new Gson();
        final RecordRenderedExpense record = gson.fromJson(intent.getStringExtra("expense"), RecordRenderedExpense.class);
        final RenderedExpense expense = record.getData();

        String action = intent.getStringExtra("action");
        Log.d(TAG, "onReceive: action = " + action + ", expense " + record.getId());

        if (action.equals("mark")) {
            expense.setLastModifiedThrough("moneydb-app");

            if (expense.getFlags().contains(ExpenseFlagging.NEEDSATTENTION))
                expense.getFlags().remove(ExpenseFlagging.NEEDSATTENTION);
            else
                expense.getFlags().add(ExpenseFlagging.NEEDSATTENTION);

            try {
                Api.getInstance(context).apiExpensesRenderedIdPutAsync(expense, record.getId().intValue(), new NoStatusApiCallback<Void>() {
                    @Override
                    public void onFailure(ApiException e, int i, Map<String, List<String>> map) {
                        Log.e(TAG, "Failed to mark expense " + record.getId(), e);
                        Log.e(TAG, gson.toJson(expense));
                    }

                    @Override
                    public void onSuccess(Void aVoid, int i, Map<String, List<String>> map) {
                        Log.d(TAG, "Marked expense " + record.getId());

                        (new UpdateHandler(context)).pullExpense(record.getId(), null, ExpenseNotificationBuilder.ExpenseSource.PULL_REFRESH);
                    }
                });
            } catch (Exception ex) {
                Log.e(TAG, "Marking Expense " + record.getId() + " failed", ex);
            }
        } else if (action.equals("comment")) {
            CharSequence comment = getCommentText(intent);
            Log.d(TAG, "Adding Comment '" + comment + "'");

            expense.setLastModifiedThrough("moneydb-app");
            if (expense.getComments().length() == 0)
                expense.setComments(comment.toString());
            else
                expense.setComments(expense.getComments() + "\n" + comment);

            try {
                Api.getInstance(context).apiExpensesRenderedIdPutAsync(expense, record.getId().intValue(), new NoStatusApiCallback<Void>() {
                    @Override
                    public void onFailure(ApiException e, int i, Map<String, List<String>> map) {
                        Log.e(TAG, "Failed to comment expense " + record.getId(), e);

                        (new UpdateHandler(context)).pullExpense(record.getId(), null, ExpenseNotificationBuilder.ExpenseSource.PULL_REFRESH);
                    }

                    @Override
                    public void onSuccess(Void aVoid, int i, Map<String, List<String>> map) {
                        Log.d(TAG, "commented expense " + record.getId());

                        (new UpdateHandler(context)).pullExpense(record.getId(), null, ExpenseNotificationBuilder.ExpenseSource.PULL_REFRESH);
                    }
                });
            } catch (Exception ex) {
                Log.e(TAG, "Adding comment to expense " + record.getId() + " failed", ex);
            }
        }


        // close the notification tray
        // Intent it = new Intent(Intent.ACTION_CLOSE_SYSTEM_DIALOGS);
        // context.sendBroadcast(it);
    }


}