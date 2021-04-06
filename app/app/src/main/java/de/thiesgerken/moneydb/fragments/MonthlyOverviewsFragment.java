package de.thiesgerken.moneydb.fragments;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.ListView;

import de.thiesgerken.moneydb.R;

public class MonthlyOverviewsFragment extends UpdatableFragment {
    private View frag;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        frag = inflater.inflate(R.layout.fragment_list, container, false);

        updateData();

        ListView listView = (ListView) frag.findViewById(R.id.fragment_list);
        listView.setOnItemClickListener(new AdapterView.OnItemClickListener() {
            @Override
            public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
                // DO something if the user clicked on the item
            }
        });

        return frag;
    }

    @Override
    public void updateData() {
//            ListView listView = (ListView) frag.findViewById(R.id.fragment_list);
//        MonthlyOverviewsArrayAdapter adapter = new MonthlyOverviewsArrayAdapter(frag.getContext(), ApiClient.getOverview().getMonthlyOverviews());
//        listView.setAdapter(adapter);
    }

    @Override
    public String getTitle() {
        return "Monatliche Übersichten";
    }

}