package de.thiesgerken.moneydb.fragments;

import android.app.Fragment;

public abstract class UpdatableFragment extends Fragment {
    public abstract void updateData();

    public abstract String getTitle();
}
