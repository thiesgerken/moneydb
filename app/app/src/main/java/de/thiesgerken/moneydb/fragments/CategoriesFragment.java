package de.thiesgerken.moneydb.fragments;

import android.os.Bundle;
import android.support.v4.widget.SwipeRefreshLayout;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.ListView;

import de.thiesgerken.moneydb.Api;
import de.thiesgerken.moneydb.R;

public class CategoriesFragment extends UpdatableFragment {
    private View frag;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        frag = inflater.inflate(R.layout.fragment_list, container, false);

        SwipeRefreshLayout swipe = (SwipeRefreshLayout) frag.findViewById(R.id.fragment_refresh);
        swipe.setEnabled(false);
        updateData();

        ListView list = (ListView) frag.findViewById(R.id.fragment_list);
        list.setOnItemClickListener(new AdapterView.OnItemClickListener() {
            @Override
            public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
                // DO something if the user clicked on the item
            }
        });

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
            }, null);
        else
            getData();
    }

    private void getData() {
        final ListView list = (ListView) frag.findViewById(R.id.fragment_list);
        final CategoriesArrayAdapter adapter = new CategoriesArrayAdapter(frag.getContext(), Api.getCategories());

        list.post(new Runnable() {
            @Override
            public void run() {
                list.setAdapter(adapter);
            }
        });

    }

    @Override
    public String getTitle() {
        return "Kategorien";
    }
}