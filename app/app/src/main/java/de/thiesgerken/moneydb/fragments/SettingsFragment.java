package de.thiesgerken.moneydb.fragments;

import android.os.Bundle;
import android.preference.PreferenceFragment;

import de.thiesgerken.moneydb.R;

/**
 * Created by thies on 14.04.17.
 */

public class SettingsFragment extends PreferenceFragment {
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        // Load the preferences from an XML resource
        addPreferencesFromResource(R.xml.preferences);
    }
}
