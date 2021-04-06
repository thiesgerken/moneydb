package de.thiesgerken.moneydb.fragments;

import android.content.Context;
import android.graphics.Color;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.ImageView;
import android.widget.ListAdapter;
import android.widget.TextView;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import de.thiesgerken.moneydb.Api;
import de.thiesgerken.moneydb.R;
import io.swagger.client.model.RecordAccount;

public class AccountsArrayAdapter extends BaseAdapter implements ListAdapter {
    private final Context context;
    private final List<RecordAccount> values;

    public AccountsArrayAdapter(Context context, List<RecordAccount> vals) {
        this.context = context;
        this.values = new ArrayList<>();

        for (RecordAccount a : vals)
            if (!a.getData().isHidden()) values.add(a);

        Collections.sort(this.values, new Comparator<RecordAccount>() {
            @Override
            public int compare(RecordAccount itm2, RecordAccount itm1) {
                int tc = itm1.getData().getAvailability().compareTo(itm2.getData().getAvailability());
                if (tc != 0) return -tc;

                tc = itm1.getData().getKind().compareTo(itm2.getData().getKind());
                if (tc != 0) return tc;

                return itm1.getData().getTitle().compareTo(itm2.getData().getTitle());
            }
        });
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
        return position;
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        View rowView;

        if (convertView == null) {
            LayoutInflater inflater = (LayoutInflater) context
                    .getSystemService(Context.LAYOUT_INFLATER_SERVICE);

            rowView = inflater.inflate(R.layout.row, parent, false);
        } else
            rowView = convertView;

        TextView titleView = rowView.findViewById(R.id.title);
        TextView amountView = rowView.findViewById(R.id.value);
        TextView descView = rowView.findViewById(R.id.desc);
        ImageView typeView = rowView.findViewById(R.id.image);

        RecordAccount val = values.get(position);

        titleView.setText(val.getData().getTitle());
        amountView.setText("");

        String desc = context.getResources().getString(Api.availabilityToString(val.getData().getAvailability()));

        if (val.getData().getDescription().length() != 0)
            desc += " | " + val.getData().getDescription();

        descView.setText(desc, TextView.BufferType.SPANNABLE);
        typeView.setImageResource(Api.accountToIcon(val.getData()));
        typeView.setColorFilter(Color.parseColor(val.getData().getColor()));

        return rowView;
    }
}
