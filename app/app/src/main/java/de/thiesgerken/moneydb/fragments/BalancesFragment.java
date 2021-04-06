package de.thiesgerken.moneydb.fragments;

import android.content.Context;
import android.content.DialogInterface;
import android.os.Bundle;
import android.support.v4.widget.SwipeRefreshLayout;
import android.support.v7.app.AlertDialog;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.ListView;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import de.thiesgerken.moneydb.Api;
import de.thiesgerken.moneydb.R;
import de.thiesgerken.moneydb.util.NoStatusApiCallback;
import de.thiesgerken.moneydb.util.ReloadOnScrollListener;
import de.thiesgerken.moneydb.util.RunnableArg;
import io.swagger.client.ApiException;
import io.swagger.client.model.BalanceQueryOptions;
import io.swagger.client.model.Order;
import io.swagger.client.model.Query;
import io.swagger.client.model.QueryBalanceQueryOptions;
import io.swagger.client.model.QueryColumn;
import io.swagger.client.model.QueryResponseBalance;
import io.swagger.client.model.RecordBalance;
import io.swagger.client.model.SearchOptions;

import static android.content.DialogInterface.BUTTON_POSITIVE;

public class BalancesFragment extends UpdatableFragment {

    private final String TAG = "BalancesFragment";
    private View frag;
    private BalancesArrayAdapter adapter;
    private SwipeRefreshLayout swipe;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        final Context con = this.getActivity();
        frag = inflater.inflate(R.layout.fragment_list, container, false);

        swipe = (SwipeRefreshLayout) frag.findViewById(R.id.fragment_refresh);
        swipe.setOnRefreshListener(new SwipeRefreshLayout.OnRefreshListener() {
            @Override
            public void onRefresh() {
                adapter.clear();
                adapter.notifyDataSetChanged();
                updateData();
            }
        });

        ListView balancesView = (ListView) frag.findViewById(R.id.fragment_list);
        balancesView.setOnItemClickListener(new AdapterView.OnItemClickListener() {
            @Override
            public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
                // DO something if the user clicked on the item
                // Intent myIntent = new Intent(con, ExpenseActivity.class);
                // myIntent.putExtra(ExpenseActivity.EXPENSEINDEX, (int) id);
                // startActivity(myIntent);
            }
        });

        balancesView.setOnScrollListener(new ReloadOnScrollListener(new Runnable() {
            @Override
            public void run() {
                updateData();
            }
        }));

        adapter = new BalancesArrayAdapter(frag.getContext(), new ArrayList<RecordBalance>());
        balancesView.setAdapter(adapter);
        updateData();

        return frag;
    }

    @Override
    public void updateData() {
        if (Api.shouldRefresh())
            Api.refresh(frag.getContext(), new Runnable() {
                @Override
                public void run() {
                    getData();
                }
            }, new RunnableArg<Exception>() {
                @Override
                public void run() {
                    frag.post(new Runnable() {
                        @Override
                        public void run() {
                            swipe.setRefreshing(false);
                        }
                    });
                }
            });
        else
            getData();
    }

    private void getData() {
        try {
            int offset = adapter.getBalanceCount();

            QueryBalanceQueryOptions x = new QueryBalanceQueryOptions();
            x.addColumnsItem(new QueryColumn().data("id").name("").searchable(true).orderable(true).search(new SearchOptions().regex(false).value("")));
            x.addColumnsItem(new QueryColumn().data("date").name("").searchable(true).orderable(true).search(new SearchOptions().regex(false).value("")));
            x.addOrderItem(new Order().column(1).dir("desc"));
            x.addOrderItem(new Order().column(0).dir("desc"));
            x.draw(1).start(offset).length(30).search(new SearchOptions().value("").regex(false));
            x.extra(new BalanceQueryOptions().accounts(new ArrayList<Long>()));

            Api.getInstance(frag.getContext()).apiBalancesQueryPostAsync(x, new NoStatusApiCallback<QueryResponseBalance>() {
                @Override
                public void onFailure(final ApiException e, int i, Map<String, List<String>> map) {
                    frag.post(new Runnable() {
                        @Override
                        public void run() {
                            Log.e(TAG, "Error loading balances", e);
                            Log.e(TAG, e.getResponseBody());

                            adapter.notifyDataSetChanged();
                            swipe.setRefreshing(false);
                        }
                    });
                }

                @Override
                public void onSuccess(final QueryResponseBalance queryResponseBalance, int i, Map<String, List<String>> map) {
                    frag.post(new Runnable() {
                        @Override
                        public void run() {
                            adapter.addItems(queryResponseBalance.getData());
                            adapter.notifyDataSetChanged();
                            swipe.setRefreshing(false);
                        }
                    });
                }
            });

        } catch (ApiException e) {
            Log.e(TAG, "Error loading balances", e);

            frag.post(new Runnable() {
                @Override
                public void run() {
                    final AlertDialog myDialog = new AlertDialog.Builder(frag.getContext()).create();
                    myDialog.setTitle("Fehler");
                    myDialog.setMessage("Konnte Kontostände nicht laden");
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
    }

    @Override
    public String getTitle() {
        return "Kontostände";
    }
}