package de.thiesgerken.moneydb.fragments;

import android.content.Context;
import android.graphics.Color;
import android.support.v4.content.res.ResourcesCompat;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.ImageView;
import android.widget.ListAdapter;
import android.widget.TextView;

import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;

import java.util.ArrayList;
import java.util.List;

import de.thiesgerken.moneydb.Api;
import de.thiesgerken.moneydb.R;
import io.swagger.client.model.Balance;
import io.swagger.client.model.RecordBalance;

import static android.view.View.GONE;

public class BalancesArrayAdapter extends BaseAdapter implements ListAdapter {
    private final Context context;
    private final List<RecordBalance> values;
    private String lastDate = "";
    private int balanceCount = 0;

    public BalancesArrayAdapter(Context context, List<RecordBalance> vals) {
        this.context = context;
        this.values = new ArrayList<>();

        addItems(vals);
    }

    public void clear() {
        lastDate = "";
        balanceCount = 0;
        values.clear();
    }

    public void addItems(List<RecordBalance> items) {
        DateTimeFormatter fmt = DateTimeFormat.forPattern("dd.MM.y");

        for (RecordBalance val : items) {
            String dt = fmt.print(new DateTime(val.getData().getDate()));

            if (!dt.equals(lastDate)) {
                lastDate = dt;

                Balance div = new Balance();
                div.setDate(val.getData().getDate());

                RecordBalance divr = new RecordBalance();
                divr.setId(-1L);
                divr.setData(div);

                values.add(divr);
            }

            values.add(val);
        }

        balanceCount += items.size();
    }

    @Override
    public int getItemViewType(int position) {
        // Define a way to determine which layout to use, here it's just evens and odds.
        return values.get(position).getId() == -1L ? 1 : 0;
    }

    @Override
    public int getViewTypeCount() {
        return 2;
    }

    @Override
    public boolean isEnabled(int position) {
        return values.get(position).getId() != -1L;
    }

    @Override
    public void notifyDataSetChanged() {
        super.notifyDataSetChanged();
    }

    @Override
    public int getCount() {
        return values.size();
    }

    @Override
    public Object getItem(int position) {
        return values.get(position);
    }

    @Override
    public long getItemId(int position) {
        return values.get(position).getId();
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        View rowView;
        RecordBalance val = values.get(position);
        Balance bal = val.getData();

        if (val.getId() != -1) {
            if (convertView == null) {
                LayoutInflater inflater = (LayoutInflater) context
                        .getSystemService(Context.LAYOUT_INFLATER_SERVICE);

                rowView = inflater.inflate(R.layout.row, parent, false);
            } else
                rowView = convertView;

            // Displaying a textview
            TextView titleView =  rowView.findViewById(R.id.title);
            TextView amountView =  rowView.findViewById(R.id.value);
            TextView descView =  rowView.findViewById(R.id.desc);
            ImageView typeView =  rowView.findViewById(R.id.image);

            titleView.setText(Api.getAccount(bal).getTitle());
            amountView.setText(String.format("%4.2f€", bal.getAmount()).replace('.', ','));

            if (bal.getAmount() < -1e-3)
                amountView.setTextColor(ResourcesCompat.getColor(context.getResources(), R.color.accent, null));
            else if (bal.getAmount() > 1e-3)
                amountView.setTextColor(ResourcesCompat.getColor(context.getResources(), R.color.primary, null));
            else
                amountView.setTextColor(ResourcesCompat.getColor(context.getResources(), R.color.secondary_text, null));

            descView.setVisibility(GONE);

            typeView.setImageResource(Api.accountToIcon(Api.getAccount(bal)));
            typeView.setColorFilter(Color.parseColor(Api.getAccount(bal.getAccountId()).getColor()));
        } else {
            if (convertView == null) {
                LayoutInflater inflater = (LayoutInflater) context
                        .getSystemService(Context.LAYOUT_INFLATER_SERVICE);

                rowView = inflater.inflate(R.layout.divider, parent, false);
            } else
                rowView = convertView;

            TextView titleView =  rowView.findViewById(R.id.dividertitle);
            titleView.setText(DateTimeFormat.forPattern("dd.MM.y").print(new DateTime(bal.getDate())));
        }

        return rowView;
    }

    public int getBalanceCount() {
        return balanceCount;
    }
}