package de.thiesgerken.moneydb.fragments;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.ListView;

import de.thiesgerken.moneydb.R;

public class CurrentBalancesFragment extends UpdatableFragment {
    View frag;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        frag = inflater.inflate(R.layout.fragment_list, container, false);

        updateData();

        ListView currentBalancesView = (ListView) frag.findViewById(R.id.fragment_list);
        currentBalancesView.setOnItemClickListener(new AdapterView.OnItemClickListener() {
            @Override
            public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
                // DO something if the user clicked on the item
            }
        });

        return frag;
    }

    @Override
    public void updateData() {
//        ListView currentBalancesView = (ListView) frag.findViewById(R.id.fragment_list);
//        CurrentBalancesArrayAdapter adapter = new CurrentBalancesArrayAdapter(frag.getContext(), ApiClient.getOverview().getCurrentBalances());
//        currentBalancesView.setAdapter(adapter);
    }

    @Override
    public String getTitle() {
        return "Aktuelle Kontostände";
    }

}