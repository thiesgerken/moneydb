/*
package de.thiesgerken.moneydb.fragments;

import android.content.Context;
import android.support.v4.content.res.ResourcesCompat;
import android.text.Spannable;
import android.text.style.ForegroundColorSpan;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.ImageView;
import android.widget.ListAdapter;
import android.widget.TextView;

import static android.view.View.GONE;

public class MonthlyOverviewsArrayAdapter extends BaseAdapter implements ListAdapter {
    private final Context context;
    private final ArrayList<Overview> values;

    public MonthlyOverviewsArrayAdapter(Context context, ArrayList<Overview> values) {

        this.context = context;
        this.values = values;

        // Sorting
//        Collections.sort(this.values, new Comparator<Overview>() {
//            @Override
//            public int compare(Overview bal2, Overview bal1) {
//                return bal1.getStartDate().compareTo(bal2.getStartDate());
//            }
//        });
    }

    private static int amountToColor(Context context, double amount) {
        if (amount < -1e-3)
            return ResourcesCompat.getColor(context.getResources(), de.thiesgerken.moneydb.R.color.accent, null);
        else if (amount > 1e-3)
            return ResourcesCompat.getColor(context.getResources(), de.thiesgerken.moneydb.R.color.primary, null);
        else
            return ResourcesCompat.getColor(context.getResources(), de.thiesgerken.moneydb.R.color.secondary_text, null);
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

            rowView = inflater.inflate(de.thiesgerken.moneydb.R.layout.row, parent, false);
        } else
            rowView = convertView;

        // Displaying a textview
        TextView titleView = (TextView) rowView.findViewById(de.thiesgerken.moneydb.R.id.title);
        TextView amountView = (TextView) rowView.findViewById(de.thiesgerken.moneydb.R.id.value);
        TextView descView = (TextView) rowView.findViewById(de.thiesgerken.moneydb.R.id.desc);
        ImageView typeView = (ImageView) rowView.findViewById(de.thiesgerken.moneydb.R.id.image);

        Overview val = values.get(position);
//        SimpleDateFormat fmt = new SimpleDateFormat("dd.MM.yyyy");

        double sumEnd = val.getEndBalances().getTotal();
        double sumEndSim = val.getEndBalancesSim().getTotal();
        double sumStart = val.getStartBalances().getTotal();

        double diff = sumEnd - sumStart;

        titleView.setText(val.getStartTime() + " bis " + val.getEndTime());
        amountView.setText(String.format("%4.2f€", diff));
        amountView.setTextColor(amountToColor(context, diff));

        String startStr = String.format("%4.2f€", sumStart);
        String endStr = String.format("%4.2f€", sumEnd);
        String endSimStr = String.format("%4.2f€", sumEndSim);

        String desc = "Anfang: " + startStr + " Ende: " + endStr + " (gedacht " + endSimStr + ")";
        descView.setText(desc, TextView.BufferType.SPANNABLE);

        Spannable str = (Spannable) descView.getText();
        str.setSpan(new ForegroundColorSpan(amountToColor(context, sumStart)), 8, 8 + startStr.length(), Spannable.SPAN_EXCLUSIVE_EXCLUSIVE);
        str.setSpan(new ForegroundColorSpan(amountToColor(context, sumEnd)), 8 + startStr.length() + 7, 8 + startStr.length() + 7 + endStr.length(), Spannable.SPAN_EXCLUSIVE_EXCLUSIVE);
        str.setSpan(new ForegroundColorSpan(amountToColor(context, sumEndSim)), 8 + startStr.length() + 7 + endStr.length() + 10, 8 + startStr.length() + 7 + endStr.length() + 10 + endSimStr.length(), Spannable.SPAN_EXCLUSIVE_EXCLUSIVE);

        typeView.setVisibility(GONE);

        return rowView;
    }
}
*/
