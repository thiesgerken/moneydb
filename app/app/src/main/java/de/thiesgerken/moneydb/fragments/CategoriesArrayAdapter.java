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

import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import de.thiesgerken.moneydb.R;
import io.swagger.client.model.Category;
import io.swagger.client.model.RecordCategory;

import static android.view.View.GONE;
import static android.view.View.VISIBLE;

public class CategoriesArrayAdapter extends BaseAdapter implements ListAdapter {
    private final Context context;
    private final List<RecordCategory> values;

    public CategoriesArrayAdapter(Context context, List<RecordCategory> values) {
        this.context = context;
        this.values = values;

        // Sorting
        Collections.sort(this.values, new Comparator<RecordCategory>() {
            @Override
            public int compare(RecordCategory cat2, RecordCategory cat1) {
                return cat1.getData().getTitle().compareTo(cat2.getData().getTitle());
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

        Category val = values.get(position).getData();

        titleView.setText(val.getTitle());
        amountView.setText("");

        if (val.getDescription().length() == 0)
            descView.setVisibility(GONE);
        else
            descView.setVisibility(VISIBLE);

        descView.setText(val.getDescription());

        typeView.setColorFilter(Color.parseColor(val.getColor()));
        typeView.setImageResource(R.drawable.ic_bookmark_black_24px);

        return rowView;
    }
}
