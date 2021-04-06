package de.thiesgerken.moneydb.notifications;

import android.app.Notification;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.app.RemoteInput;
import android.content.Context;
import android.content.Intent;
import android.graphics.Color;
import android.graphics.Typeface;
import android.graphics.drawable.Icon;
import android.media.AudioAttributes;
import android.media.RingtoneManager;
import android.text.Spannable;
import android.text.SpannableString;
import android.text.SpannableStringBuilder;
import android.text.style.ForegroundColorSpan;
import android.text.style.StyleSpan;
import android.text.style.TypefaceSpan;

import com.google.gson.Gson;

import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;

import de.thiesgerken.moneydb.Api;
import de.thiesgerken.moneydb.MainActivity;
import de.thiesgerken.moneydb.R;
import io.swagger.client.model.ExpenseFlagging;
import io.swagger.client.model.RecordRenderedExpense;
import io.swagger.client.model.RenderedExpense;

/**
 * Created by thies on 30.04.17.
 */

public final class ExpenseNotificationBuilder {
    private static final String GROUP_KEY = "Expenses";
    private static final int SUMMARY_ID = -100;
    public static final String KEY_TEXT_COMMENT = "key_text_comment";

    private final Context context;
    private final NotificationManager notificationManager;

    private NotificationChannel notificationChannel;

    public enum ExpenseSource {
        PULL_PERIODIC,
        PULL_REFRESH,
        PUSH_SINGLE,
        PUSH_UPDATES
    }

    private ExpenseNotificationBuilder(Context context,
                                       NotificationManager notificationManager) {
        this.context = context.getApplicationContext();
        this.notificationManager = notificationManager;

        notificationChannel = new NotificationChannel("expenses", "Expenses", NotificationManager.IMPORTANCE_LOW);
        notificationChannel.enableVibration(true);
        notificationChannel.setVibrationPattern(new long[]{100, 200, 300, 400, 500, 400, 300, 200, 400});
        notificationChannel.setSound(RingtoneManager.getDefaultUri(RingtoneManager.TYPE_NOTIFICATION), new AudioAttributes.Builder().build());

        notificationManager.createNotificationChannel(notificationChannel);
    }

    public static ExpenseNotificationBuilder newInstance(Context context) {
        Context appContext = context.getApplicationContext();
        Context safeContext = appContext.createDeviceProtectedStorageContext();
        if (safeContext == null) {
            safeContext = appContext;
        }

        NotificationManager notificationManager =
                (NotificationManager) safeContext.getSystemService(Context.NOTIFICATION_SERVICE);

        return new ExpenseNotificationBuilder(safeContext, notificationManager);
    }

    public void sendNotification(RecordRenderedExpense record, ExpenseSource source) {
        Notification notification = buildNotification(record, GROUP_KEY, source);
        notificationManager.notify(record.getId().intValue(), notification);

        if (android.os.Build.VERSION.SDK_INT >= 25) {
            Notification summary = buildSummary(GROUP_KEY);
            notificationManager.notify(SUMMARY_ID, summary);
        }
    }

    private Notification buildNotification(RecordRenderedExpense record, String groupKey, ExpenseSource source) {
        final RenderedExpense exp = record.getData();

        Spannable amountText = new SpannableString(Api.formatAmount(exp.getEffectiveAmount()));

        if (exp.getEffectiveAmount() != 0)
            amountText.setSpan(new ForegroundColorSpan(context.getResources().getColor(exp.getEffectiveAmount() > 0 ? R.color.accent : R.color.primary, null)), 0, amountText.length(), Spannable.SPAN_EXCLUSIVE_EXCLUSIVE);

        Spannable accountText = new SpannableString(Api.getTitle(Api.getAccount(exp.getAccountId())));
        accountText.setSpan(new ForegroundColorSpan(Color.parseColor(Api.getAccount(exp.getAccountId()).getColor())), 0, accountText.length(), Spannable.SPAN_EXCLUSIVE_EXCLUSIVE);

        Spannable categoryText = new SpannableString(Api.getTitle(Api.getCategory(exp.getCategoryId())));
        categoryText.setSpan(new ForegroundColorSpan(Color.parseColor(Api.getCategory(exp.getCategoryId()).getColor())), 0, categoryText.length(), Spannable.SPAN_EXCLUSIVE_EXCLUSIVE);

        SpannableStringBuilder contentText = new SpannableStringBuilder();
        contentText.append(amountText);

        DateTimeFormatter ptn = DateTimeFormat.forPattern("d. MMMM");
        contentText.append(" auf ").append(accountText);

        if (!ptn.print(new DateTime(exp.getValueDate())).equals(ptn.print(new DateTime())))
            contentText.append(" am " + ptn.print(new DateTime(exp.getValueDate())));
        if (exp.getStore().length() != 0)
            contentText.append(" bei ").append(exp.getStore());

        SpannableStringBuilder bigText = new SpannableStringBuilder();
        bigText.append(contentText);

        if (exp.getLastModified().equals(exp.getCreationDate()))
            bigText.append(", erstellt ");
        else
            bigText.append(", geändert ");

        if (exp.getLastModifiedBy() != Api.getLoggedInUser().getId())
            bigText.append("von ").append(Api.getUser(exp.getLastModifiedBy()).getFullName()).append(" ");

        Spannable throughText = new SpannableString(exp.getLastModifiedThrough());
        throughText.setSpan(new TypefaceSpan("monospace"), 0, throughText.length(), Spannable.SPAN_EXCLUSIVE_EXCLUSIVE);
        bigText.append("durch ").append(throughText).append(". ");

        // bigText.append("Zusammengesetzt aus ").append(Api.getSharingDescription(context, exp)).append(".");

        bigText.append("\n");

        if (exp.getFlags().contains(ExpenseFlagging.NEEDSATTENTION)) {
            Spannable attentionText = new SpannableString("Benötigt Aufmerksamkeit");
            attentionText.setSpan(new StyleSpan(Typeface.BOLD), 0, attentionText.length(), Spannable.SPAN_EXCLUSIVE_EXCLUSIVE);
            bigText.append("\n").append(attentionText).append("\n");
        }

        if (exp.getDescription().length() != 0)
            bigText.append("\n").append(exp.getDescription()).append("\n");

        if (exp.getComments().length() != 0)
            bigText.append("\n").append(exp.getComments()).append("\n");

        Spannable transactionText = new SpannableString(exp.getTransaction().replaceAll("\\s+", " ").replace(" ", "\u00A0"));
        transactionText.setSpan(new TypefaceSpan("monospace"), 0, transactionText.length(), Spannable.SPAN_EXCLUSIVE_EXCLUSIVE);
        // transactionText.setSpan(new RelativeSizeSpan(0.4f), 0, transactionText.length(), Spannable.SPAN_EXCLUSIVE_EXCLUSIVE);

        if (exp.getTransaction().length() != 0)
            bigText.append("\n").append(transactionText).append("\n");

        bigText.delete(bigText.length() - 1, bigText.length());

        String title = exp.getTitle();

        PendingIntent contentIntent = PendingIntent.getActivity(context, 0, new Intent(context, MainActivity.class), 0);

        Notification.Builder builder = new Notification.Builder(context, notificationChannel.getId())
                .setWhen(new DateTime().getMillis())
                .setShowWhen(true)
                .setCategory(Notification.CATEGORY_EVENT)
                .setSmallIcon(Api.accountToIcon(Api.getAccount(exp.getAccountId())))
                .setSubText(categoryText)
                .setContentTitle(title)
                .setStyle(new Notification.BigTextStyle().bigText(bigText))
                .setContentText(contentText)
                .setContentIntent(contentIntent)
                .setGroup(groupKey);

        if (android.os.Build.VERSION.SDK_INT >= 25) {
            RemoteInput remoteInput = new RemoteInput.Builder(KEY_TEXT_COMMENT)
                    .setLabel("Kommentar hinzufügen").build();

            Intent intentComment = new Intent(context, ExpenseReceiver.class);
            intentComment.putExtra("action", "comment");
            intentComment.putExtra("expense", (new Gson()).toJson(exp));

            PendingIntent pIntentComment = PendingIntent.getBroadcast(context, 1, intentComment, PendingIntent.FLAG_UPDATE_CURRENT);

            Notification.Action commentAction =
                    new Notification.Action.Builder(Icon.createWithResource(context, R.drawable.ic_euro_symbol),
                            "Kommentieren", pIntentComment).addRemoteInput(remoteInput).build();

            builder.addAction(commentAction);
        }

        Intent intentMark = new Intent(context, ExpenseReceiver.class);
        intentMark.putExtra("action", "mark");
        intentMark.putExtra("expense", (new Gson()).toJson(exp));

        PendingIntent pIntentMark = PendingIntent.getBroadcast(context, 2, intentMark, PendingIntent.FLAG_UPDATE_CURRENT);

        Notification.Action markAction =
                new Notification.Action.Builder(Icon.createWithResource(context, R.drawable.ic_refresh),
                        exp.getFlags().contains(ExpenseFlagging.NEEDSATTENTION) ? "Korrekt" : "Nicht korrekt", pIntentMark).build();

        builder.addAction(markAction).setChannelId(notificationChannel.getId());

        return builder.build();
    }

    private Notification buildSummary(String groupKey) {
        return new Notification.Builder(context, notificationChannel.getId())
                .setContentTitle("Ausgabenverwaltung")
                .setContentText("Es gibt neue Ausgaben")
                .setWhen(System.currentTimeMillis())
                .setSmallIcon(R.drawable.ic_euro_symbol)
                .setShowWhen(true)
                .setGroup(groupKey)
                .setGroupSummary(true)
                .build();
    }
}