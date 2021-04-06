package de.thiesgerken.moneydb;

import android.content.Context;
import android.content.SharedPreferences;
import android.graphics.Color;
import android.preference.PreferenceManager;
import android.support.v4.content.res.ResourcesCompat;
import android.text.Spannable;
import android.text.SpannableString;
import android.text.SpannableStringBuilder;
import android.text.style.ForegroundColorSpan;
import android.util.Log;

import org.joda.time.DateTime;
import org.joda.time.Hours;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import de.thiesgerken.moneydb.util.RunnableArg;
import io.swagger.client.api.DefaultApi;
import io.swagger.client.model.Account;
import io.swagger.client.model.Availability;
import io.swagger.client.model.Balance;
import io.swagger.client.model.Category;
import io.swagger.client.model.RecordAccount;
import io.swagger.client.model.RecordCategory;
import io.swagger.client.model.RecordRenderedUser;
import io.swagger.client.model.RenderedExpense;
import io.swagger.client.model.RenderedSharing;
import io.swagger.client.model.RenderedUser;

import com.squareup.okhttp.OkHttpClient;
import com.squareup.okhttp.Protocol;

/**
 * Created by thies on 08.04.17.
 */

public class Api {
    private static final String TAG = "Api";

    private static DefaultApi instance = new DefaultApi();

    private static List<RecordAccount> visibleAccounts;
    private static List<RecordCategory> visibleCategories;
    private static List<RecordRenderedUser> visibleUsers;

    private static List<RecordAccount> accounts;
    private static List<RecordCategory> categories;

    private static DateTime lastRefresh = null;

    private static RecordRenderedUser me;

    public static void maybeRefresh(final Context con, final Runnable successCallback, final Runnable failureCallback) {
        if (shouldRefresh())
            refresh(con, successCallback, failureCallback);
        else
            successCallback.run();
    }

    private static void initializeInstance(Context con) {
        SharedPreferences sharedPref = PreferenceManager.getDefaultSharedPreferences(con);

        instance.getApiClient().setHttpClient(new OkHttpClient().setProtocols(Arrays.asList(Protocol.HTTP_1_1)));

        instance.getApiClient().setUsername(sharedPref.getString("pref_user", null));
        instance.getApiClient().setPassword(sharedPref.getString("pref_password", null));
        instance.getApiClient().setBasePath(sharedPref.getString("pref_url", null));
    }

    public static void refresh(final Context con, final Runnable successCallback, final Runnable failureCallback) {
        new Thread(new Runnable() {
            public void run() {
                try {
                    initializeInstance(con);

                    Log.d(TAG, "Refreshing Accounts, Categories and Users");
                    visibleAccounts = instance.apiAccountsGet(0, null);
                    visibleCategories = instance.apiCategoriesGet(0, null);
                    visibleUsers = instance.apiUsersGet(0, null);
                    me = instance.apiUsersMeGet();

                    accounts = new ArrayList<>();
                    categories = new ArrayList<>();

                    for (RecordAccount a : visibleAccounts)
                        if (a.getData().getOwnerId() == me.getId())
                            accounts.add(a);

                    for (RecordCategory c : visibleCategories)
                        if (c.getData().getOwnerId() == me.getId())
                            categories.add(c);

                    lastRefresh = new DateTime();
                    if (successCallback != null)
                        successCallback.run();
                } catch (Exception e) {
                    Log.e(TAG, "Error refreshing Accounts, Categories and Users", e);
                    if (failureCallback != null)
                        if (failureCallback instanceof RunnableArg)
                            ((RunnableArg<Exception>) failureCallback).run(e);
                        else
                            failureCallback.run();
                }
            }
        }).start();
    }

    public static Account getAccount(long id) {
        for (RecordAccount a : visibleAccounts)
            if (a.getId() == id)
                return a.getData();

        return null;
    }

    public static Category getCategory(long id) {
        for (RecordCategory a : visibleCategories)
            if (a.getId() == id)
                return a.getData();

        return null;
    }

    public static RenderedUser getUser(long id) {
        for (RecordRenderedUser a : visibleUsers)
            if (a.getId() == id)
                return a.getData();

        return null;
    }

    public static Account getAccount(Balance bal) {
        return getAccount(bal.getAccountId());
    }

    public static String getTitle(Account acc) {
        if (isMine(acc))
            return acc.getTitle();
        else
            return getUser(acc.getOwnerId()).getFullName() + " / " + acc.getTitle();
    }

    public static List<Account> getRelevantAccounts (RenderedExpense exp) {
        ArrayList<Long> res = new ArrayList<>();
              res.add(exp.getAccountId());

        for (RenderedSharing x : exp.getSharing())
            if (! res.contains(x.getSharingAccountId()))
                res.add(x.getSharingAccountId());

        ArrayList<Account> res2 = new ArrayList<>();
        for (Long i : res)
            res2.add(getAccount(i));

        return res2;
    }

    public static String getTitle(long userId, Account acc) {
        if (acc.getOwnerId() == userId)
            return acc.getTitle();
        else
            return getUser(acc.getOwnerId()).getFullName() + " / " + acc.getTitle();
    }

    public static String getTitle(Category cat) {
        if (isMine(cat))
            return cat.getTitle();
        else
            return getUser(cat.getOwnerId()).getFullName() + " / " + cat.getTitle();
    }

    private static boolean isMine(Category cat) {
        return cat.getOwnerId() == getLoggedInUser().getId();
    }

    private static boolean isMine(Account acc) {
        return acc.getOwnerId() == getLoggedInUser().getId();
    }

    public static String formatAmount(double amount) {
        return String.format("%4.2f€", amount).replace('.', ',');
    }

    private static void makeAccountLine(SpannableStringBuilder sb, Context con, long userId, long accountId, double amount, String text, String text2) {
        Account acc = getAccount(accountId);

        Spannable accText = new SpannableString(getTitle(userId, acc));
        accText.setSpan(new ForegroundColorSpan(Color.parseColor(acc.getColor())), 0, accText.length(), Spannable.SPAN_EXCLUSIVE_EXCLUSIVE);

        Spannable restText = new SpannableString(text);

        if (!isMine(getAccount(accountId)))
            amount = 0;

        if (Math.abs(amount) > 1e-4)
            restText.setSpan(new ForegroundColorSpan(ResourcesCompat.getColor(con.getResources(), amount > 0 ? R.color.accent : R.color.primary, null)), 0, restText.length(), Spannable.SPAN_EXCLUSIVE_EXCLUSIVE);

        sb.append(restText).append(" auf ").append(accText).append(text2).append(", ");
    }

    public static int accountToIcon(Account acc) {
        switch (acc.getKind()) {
            case CASH:
                return R.drawable.ic_euro_symbol;
            case DEBIT:
                return R.drawable.ic_local_atm;
            case CREDIT:
                return R.drawable.ic_credit_card;
            case DEBT:
                return R.drawable.ic_face;
            case VIRTUAL:
                return R.drawable.ic_filter_drama;
            case PREPAYMENT:
                return R.drawable.ic_filter_drama;
            case INVESTMENT:
                return R.drawable.ic_lock_outline;
        }

        return R.drawable.ic_euro_symbol;
    }

    public static int availabilityToString(Availability av) {
        switch (av) {
            case WEEKS:
                return R.string.av_weeks;
            case MONTHS:
                return R.string.av_months;
            case YEARS:
                return R.string.av_years;
            case DECADES:
                return R.string.av_decades;
        }

        return R.string.av_immediately;
    }

    public static DefaultApi getInstance(Context con) {
        initializeInstance(con);
        return instance;
    }

    public static List<RecordAccount> getVisibleAccounts() {
        return visibleAccounts;
    }

    public static List<RecordCategory> getVisibleCategories() {
        return visibleCategories;
    }

    public static List<RecordRenderedUser> getVisibleUsers() {
        return visibleUsers;
    }

    public static RecordRenderedUser getLoggedInUser() {
        return me;
    }

    public static List<RecordAccount> getAccounts() {
        return accounts;
    }

    public static List<RecordCategory> getCategories() {
        return categories;
    }

    public static boolean shouldRefresh() {
        if (lastRefresh == null)
            return true;

        return Math.abs(Hours.hoursBetween(new DateTime(), lastRefresh).getHours()) >= 24;
    }
}
