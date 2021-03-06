package de.thiesgerken.moneydb.fragments;

import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
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
import de.thiesgerken.moneydb.ExpenseActivity;
import de.thiesgerken.moneydb.R;
import de.thiesgerken.moneydb.util.NoStatusApiCallback;
import de.thiesgerken.moneydb.util.ReloadOnScrollListener;
import de.thiesgerken.moneydb.util.RunnableArg;
import io.swagger.client.ApiException;
import io.swagger.client.model.ExpenseQueryOptions;
import io.swagger.client.model.Order;
import io.swagger.client.model.QueryColumn;
import io.swagger.client.model.QueryExpenseQueryOptions;
import io.swagger.client.model.QueryResponseRenderedExpense;
import io.swagger.client.model.RecordRenderedExpense;
import io.swagger.client.model.SearchOptions;

import static android.content.DialogInterface.BUTTON_POSITIVE;

public class ExpensesFragment extends UpdatableFragment {

    private final String TAG = "ExpensesFragment";
    private View frag;
    private ExpensesArrayAdapter adapter;
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
                updateData();
            }
        });

        ListView expensesView = (ListView) frag.findViewById(R.id.fragment_list);
        expensesView.setOnItemClickListener(new AdapterView.OnItemClickListener() {
            @Override
            public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
                // DO something if the user clicked on the item

                Intent myIntent = new Intent(con, ExpenseActivity.class);
                myIntent.putExtra(ExpenseActivity.EXPENSEINDEX, (int) id);
                startActivity(myIntent);

            }
        });

        expensesView.setOnScrollListener(new ReloadOnScrollListener(new Runnable() {
            @Override
            public void run() {
                // Log.d(TAG, "ScrollListener");
                updateData();
            }
        }));

        // Log.d(TAG, "onCreateView");

        adapter = new ExpensesArrayAdapter(frag.getContext(), new ArrayList<RecordRenderedExpense>());
        expensesView.setAdapter(adapter);
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
            int offset = adapter.getExpenseCount();

            // Log.d(TAG, offset + "");

            QueryExpenseQueryOptions x = new QueryExpenseQueryOptions();
            x.addColumnsItem(new QueryColumn().data("id").name("").searchable(true).orderable(true).search(new SearchOptions().regex(false).value("")));
            x.addColumnsItem(new QueryColumn().data("valueDate").name("").searchable(true).orderable(true).search(new SearchOptions().regex(false).value("")));
            x.addColumnsItem(new QueryColumn().data("checked").name("").searchable(true).orderable(true).search(new SearchOptions().regex(false).value("")));
            x.addOrderItem(new Order().column(2).dir("desc"));
            x.addOrderItem(new Order().column(1).dir("desc"));
            x.addOrderItem(new Order().column(0).dir("desc"));
            x.draw(1).start(offset).length(30).search(new SearchOptions().value("").regex(false));
            x.extra(new ExpenseQueryOptions().accounts(new ArrayList<Long>()).categories(new ArrayList<Long>()));

            Api.getInstance(frag.getContext()).apiExpensesRenderedQueryPostAsync(x, new NoStatusApiCallback<QueryResponseRenderedExpense>() {
                @Override
                public void onFailure(ApiException e, int i, Map<String, List<String>> map) {
                    frag.post(new Runnable() {
                        @Override
                        public void run() {
                            adapter.notifyDataSetChanged();
                            swipe.setRefreshing(false);
                        }
                    });
                }

                @Override
                public void onSuccess(final QueryResponseRenderedExpense queryResponseExpense, int i, Map<String, List<String>> map) {
                    frag.post(new Runnable() {
                        @Override
                        public void run() {
                            adapter.addItems(queryResponseExpense.getData());
                            adapter.notifyDataSetChanged();
                            swipe.setRefreshing(false);
                        }
                    });
                }
            });

        } catch (final Exception e) {
            Log.e(TAG, "Error loading expenses", e);

            frag.post(new Runnable() {
                @Override
                public void run() {
                    final AlertDialog myDialog = new AlertDialog.Builder(frag.getContext()).create();
                    myDialog.setTitle("Fehler");
                    myDialog.setMessage("Konnte Ausgaben nicht laden");
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
        return "Ausgaben";
    }
}