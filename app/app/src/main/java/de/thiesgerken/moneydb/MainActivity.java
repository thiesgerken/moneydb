package de.thiesgerken.moneydb;

import android.app.ActivityManager;
import android.app.AlertDialog;
import android.app.Fragment;
import android.app.FragmentManager;
import android.app.FragmentTransaction;
import android.content.DialogInterface;
import android.os.Bundle;
import android.support.design.widget.NavigationView;
import android.support.v4.view.GravityCompat;
import android.support.v4.widget.DrawerLayout;
import android.support.v7.app.ActionBarDrawerToggle;
import android.support.v7.app.AppCompatActivity;
import android.support.v7.widget.Toolbar;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.Button;
import android.widget.TextView;

import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormat;

import de.thiesgerken.moneydb.fragments.AccountsFragment;
import de.thiesgerken.moneydb.fragments.BalancesFragment;
import de.thiesgerken.moneydb.fragments.CategoriesFragment;
import de.thiesgerken.moneydb.fragments.CurrentBalancesFragment;
import de.thiesgerken.moneydb.fragments.ExpensesFragment;
import de.thiesgerken.moneydb.fragments.MonthlyOverviewsFragment;
import de.thiesgerken.moneydb.fragments.SettingsFragment;
import de.thiesgerken.moneydb.fragments.UpdatableFragment;
import de.thiesgerken.moneydb.notifications.ExpenseNotificationBuilder;
import de.thiesgerken.moneydb.notifications.UpdateAlarmReceiver;
import de.thiesgerken.moneydb.notifications.UpdateHandler;
import de.thiesgerken.moneydb.util.RunnableArg;

import static android.content.DialogInterface.BUTTON_POSITIVE;

public class MainActivity extends AppCompatActivity
        implements NavigationView.OnNavigationItemSelectedListener {

    Runnable refreshSuccessCallback = new Runnable() {
        @Override
        public void run() {
            final TextView refreshView = findViewById(R.id.navlastrefresh);
            updateHandler.registerClient();
            updateHandler.pullUpdates(ExpenseNotificationBuilder.ExpenseSource.PULL_PERIODIC, null);

            if (refreshView != null)
                refreshView.post(new Runnable() {
                    @Override
                    public void run() {
                        String lastUpdate = DateTimeFormat.forPattern("dd.MM.y HH:mm:ss").print(new DateTime());

                        refreshView.setText(refreshView.getResources().getString(R.string.last_refresh) + "\n" + lastUpdate);
                    }
                });
        }
    };

    RunnableArg<Exception> refreshFailureCallback = new RunnableArg<Exception>() {
        @Override
        public void run() {
            final Exception throwable = this.getArg();

            instance.findViewById(R.id.drawer_layout).post(new Runnable() {
                @Override
                public void run() {
                    final AlertDialog myDialog = new AlertDialog.Builder(instance).create();
                    myDialog.setTitle("Fehler");
                    myDialog.setMessage("Beim Laden der Datenbank trat ein Fehler auf: " + throwable.getMessage());
                    myDialog.setButton(BUTTON_POSITIVE, "OK", new DialogInterface.OnClickListener() {
                        @Override
                        public void onClick(DialogInterface dialog, int which) {
                            myDialog.dismiss();
                        }
                    });

                    myDialog.show();
                }
            });
        }
    };

    private UpdateAlarmReceiver alarm;
    private UpdateHandler updateHandler;
    private MainActivity instance;

    @Override
    protected void onCreate(final Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
        instance = this;

        alarm = new UpdateAlarmReceiver();
        alarm.setAlarm(this);

        updateHandler = new UpdateHandler(this);

        final Toolbar toolbar =  findViewById(R.id.toolbar);
        setSupportActionBar(toolbar);

        DrawerLayout drawer =  findViewById(R.id.drawer_layout);
        ActionBarDrawerToggle toggle = new ActionBarDrawerToggle(
                this, drawer, toolbar, R.string.navigation_drawer_open, R.string.navigation_drawer_close);
        drawer.addDrawerListener(toggle);
        toggle.syncState();

        NavigationView navigationView =  findViewById(R.id.nav_view);
        navigationView.setNavigationItemSelectedListener(this);

        getFragmentManager().addOnBackStackChangedListener(new FragmentManager.OnBackStackChangedListener() {
            public void onBackStackChanged() {
                Fragment frag = getFragmentManager().findFragmentById(R.id.main_fragment);

                if (frag instanceof UpdatableFragment)
                    toolbar.setTitle(((UpdatableFragment) frag).getTitle());
            }
        });

        final Button refreshButton =  navigationView.getHeaderView(0).findViewById(R.id.button_refresh);
        refreshButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                Api.refresh(instance, refreshSuccessCallback, refreshFailureCallback);
            }
        });

        Api.refresh(this, refreshSuccessCallback, refreshFailureCallback);
    }

    @Override
    public void onBackPressed() {
        DrawerLayout drawer = findViewById(R.id.drawer_layout);
        if (drawer.isDrawerOpen(GravityCompat.START)) {
            drawer.closeDrawer(GravityCompat.START);
        } else {
            super.onBackPressed();
        }
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        getMenuInflater().inflate(R.menu.main, menu);
        return true;
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        int id = item.getItemId();

        //noinspection SimplifiableIfStatement
        if (id == R.id.action_settings) {
            SettingsFragment newFragment = new SettingsFragment();
            FragmentTransaction transaction = getFragmentManager().beginTransaction();

            // Replace whatever is in the fragment_container view with this fragment,
            // and add the transaction to the back stack so the user can navigate back
            transaction.replace(R.id.main_fragment, newFragment);
            transaction.addToBackStack(null);

            // Commit the transaction
            transaction.commit();
            getFragmentManager().executePendingTransactions();
            ((Toolbar) findViewById(R.id.toolbar)).setTitle(R.string.prefs);
        }

        return super.onOptionsItemSelected(item);
    }

    @Override
    public boolean onNavigationItemSelected(MenuItem item) {
        // Handle navigation view item clicks here.
        int id = item.getItemId();
        UpdatableFragment newFragment = null;

        if (id == R.id.nav_currentbalances) {
            newFragment = new CurrentBalancesFragment();
        } else if (id == R.id.nav_monthly) {
            newFragment = new MonthlyOverviewsFragment();
        } else if (id == R.id.nav_accounts) {
            newFragment = new AccountsFragment();
        } else if (id == R.id.nav_balances) {
            newFragment = new BalancesFragment();
        } else if (id == R.id.nav_categories) {
            newFragment = new CategoriesFragment();
        } else if (id == R.id.nav_expenses) {
            newFragment = new ExpensesFragment();
        }

        if (newFragment != null) {
            FragmentTransaction transaction = getFragmentManager().beginTransaction();

            // Replace whatever is in the fragment_container view with this fragment,
            // and add the transaction to the back stack so the user can navigate back
            transaction.replace(R.id.main_fragment, newFragment);
            transaction.addToBackStack(null);

            // Commit the transaction
            transaction.commit();
            getFragmentManager().executePendingTransactions();
            ((Toolbar) findViewById(R.id.toolbar)).setTitle(newFragment.getTitle());
            item.setChecked(true);
        }

        ((DrawerLayout) findViewById(R.id.drawer_layout)).closeDrawer(GravityCompat.START);
        return true;
    }
}
