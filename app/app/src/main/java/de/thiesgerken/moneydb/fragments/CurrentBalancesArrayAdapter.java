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

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;

import de.thiesgerken.moneydb.Api;
import de.thiesgerken.moneydb.R;
import io.swagger.client.model.Balance;

public class CurrentBalancesArrayAdapter extends BaseAdapter implements ListAdapter {
    private final Context context;
    private final ArrayList<Balance> values;

    public CurrentBalancesArrayAdapter(Context context, ArrayList<Balance> values) {

        this.context = context;
        this.values = new ArrayList<Balance>();

        for (Balance p : values)
            if (!Api.getAccount(p).isHidden()) this.values.add(p);

        // Sorting
        Collections.sort(this.values, new Comparator<Balance>() {
            @Override
            public int compare(Balance bal2, Balance bal1) {
                int tc = Api.getAccount(bal1).getKind().compareTo(Api.getAccount(bal2).getKind());
                if (tc != 0) return tc;

                return Api.getAccount(bal1).getTitle().compareTo(Api.getAccount(bal2).getTitle());
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

        // Displaying a textview
        TextView titleView = (TextView) rowView.findViewById(R.id.title);
        TextView amountView = (TextView) rowView.findViewById(R.id.value);
        TextView descView = (TextView) rowView.findViewById(R.id.desc);
        ImageView typeView = (ImageView) rowView.findViewById(R.id.image);

        Balance val = values.get(position);

        titleView.setText(Api.getAccount(val).getTitle());
        amountView.setText(String.format("%4.2f€", val.getAmount()));

        if (val.getAmount() < 0)
            amountView.setTextColor(ResourcesCompat.getColor(context.getResources(), R.color.accent, null));
        else if (val.getAmount() > 0)
            amountView.setTextColor(ResourcesCompat.getColor(context.getResources(), R.color.primary, null));
        else
            amountView.setTextColor(ResourcesCompat.getColor(context.getResources(), R.color.secondary_text, null));

//        if (val instanceof SimulatedBalance)
//            descView.setText("basierend auf Kontostand vom " + new SimpleDateFormat("dd.MM.yyyy").format(((SimulatedBalance) val).getParent().getDate()));
//        else
//            descView.setText("exakter Kontostand");

//        typeView.setImageResource(val.getAccount().getType().toIcon());
        typeView.setColorFilter(Color.parseColor(Api.getAccount(val).getColor()));

        return rowView;
    }
}
