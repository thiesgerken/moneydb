package de.thiesgerken.moneydb.fragments;

import android.content.Context;
import android.graphics.Color;
import android.graphics.Typeface;
import android.support.v4.content.res.ResourcesCompat;
import android.text.Spannable;
import android.text.SpannableStringBuilder;
import android.text.style.ForegroundColorSpan;
import android.text.style.RelativeSizeSpan;
import android.text.style.StyleSpan;
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
import io.swagger.client.model.Account;
import io.swagger.client.model.Category;
import io.swagger.client.model.ExpenseFlagging;
import io.swagger.client.model.RecordRenderedExpense;
import io.swagger.client.model.RenderedExpense;


public class ExpensesArrayAdapter extends BaseAdapter implements ListAdapter {
    private final Context context;
    private final List<RecordRenderedExpense> values;
    private String lastDate = "";
    private int expenseCount = 0;

    public ExpensesArrayAdapter(Context context, List<RecordRenderedExpense> vals) {
        this.context = context;
        this.values = new ArrayList<>();

        addItems(vals);
    }

    public void clear() {
        lastDate = "";
        expenseCount = 0;
        values.clear();
    }

    public void addItems(List<RecordRenderedExpense> items) {
        DateTimeFormatter fmt = DateTimeFormat.forPattern("dd.MM.y");

        for (RecordRenderedExpense val : items) {
            String dt = fmt.print(new DateTime(val.getData().getValueDate()));

            if (!dt.equals(lastDate)) {
                lastDate = dt;

                RenderedExpense div = new RenderedExpense();
                div.setValueDate(val.getData().getValueDate());

                RecordRenderedExpense divr = new RecordRenderedExpense();
                divr.setId(-1L);
                divr.setData(div);

                values.add(divr);
            }

            values.add(val);
        }

        expenseCount += items.size();
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
        RecordRenderedExpense val = values.get(position);
        RenderedExpense rexp = val.getData();

        if (val.getId() != -1L) {
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

            SpannableStringBuilder title = new SpannableStringBuilder(rexp.getTitle());

            if (rexp.getFlags().contains(ExpenseFlagging.NEEDSATTENTION) && !rexp.getFlags().contains(ExpenseFlagging.TEMPLATE)) {
                title.append(" ");
                title.append("Benötigt Aufmerksamkeit", new ForegroundColorSpan(ResourcesCompat.getColor(context.getResources(), R.color.accent, null)), Spannable.SPAN_EXCLUSIVE_EXCLUSIVE);
                title.setSpan(new RelativeSizeSpan(0.75f), rexp.getTitle().length() + 1, title.length(), Spannable.SPAN_EXCLUSIVE_EXCLUSIVE);
                title.setSpan(new StyleSpan(Typeface.BOLD_ITALIC), rexp.getTitle().length() + 1, title.length(), Spannable.SPAN_EXCLUSIVE_EXCLUSIVE);
            }

            titleView.setText(title, TextView.BufferType.SPANNABLE);
            titleView.setTextColor(ResourcesCompat.getColor(context.getResources(), R.color.primary_text, null));

            amountView.setText(String.format("%4.2f€", rexp.getEffectiveAmount()).replace('.', ','));

            if (rexp.getEffectiveAmount() > 0)
                amountView.setTextColor(ResourcesCompat.getColor(context.getResources(), R.color.accent, null));
            else
                amountView.setTextColor(ResourcesCompat.getColor(context.getResources(), R.color.primary, null));

            Category cat = Api.getCategory(rexp.getCategoryId());

            SpannableStringBuilder desc = new SpannableStringBuilder();

            boolean first = true;
            for (Account a : Api.getRelevantAccounts(rexp)) {
                if (!first) desc.append(" & ");
                desc.append(Api.getTitle(a), new ForegroundColorSpan(Color.parseColor(a.getColor())), Spannable.SPAN_EXCLUSIVE_EXCLUSIVE);

                first = false;
            }

            desc.append(" | ");
            desc.append(Api.getTitle(cat), new ForegroundColorSpan(Color.parseColor(cat.getColor())), Spannable.SPAN_EXCLUSIVE_EXCLUSIVE);

            if (rexp.getStore().length() != 0)
                desc.append(" | " + rexp.getStore());

            if (rexp.getDescription().length() != 0)
                desc.append(" | " + rexp.getDescription());

            descView.setText(desc, TextView.BufferType.SPANNABLE);

            // Show the main account as icon
            Account acc = Api.getAccount(rexp.getAccountId());
            typeView.setImageResource(Api.accountToIcon(acc));
            typeView.setColorFilter(Color.parseColor(acc.getColor()));


        } else {
            if (convertView == null) {
                LayoutInflater inflater = (LayoutInflater) context
                        .getSystemService(Context.LAYOUT_INFLATER_SERVICE);

                rowView = inflater.inflate(R.layout.divider, parent, false);
            } else
                rowView = convertView;

            TextView titleView = rowView.findViewById(R.id.dividertitle);
            titleView.setText(DateTimeFormat.forPattern("dd.MM.y").print(new DateTime(rexp.getValueDate())));
        }

        return rowView;
    }

    public int getExpenseCount() {
        return expenseCount;
    }
}
