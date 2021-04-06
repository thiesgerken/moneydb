package de.thiesgerken.moneydb;

import android.content.Intent;
import android.os.Bundle;
import android.support.v4.app.NavUtils;
import android.support.v7.app.AppCompatActivity;
import android.support.v7.widget.Toolbar;
import android.view.MenuItem;

import io.swagger.client.model.Expense;

public class ExpenseActivity extends AppCompatActivity {

    public static final String EXPENSEINDEX = "idx";

    private Expense expense;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_expense);
        Toolbar toolbar = (Toolbar) findViewById(R.id.toolbar);
        setSupportActionBar(toolbar);

        getSupportActionBar().setDisplayHomeAsUpEnabled(true);

//        Bundle extras = getIntent().getExtras();
//        int expenseId;
//
//        if (extras == null) {
//            expenseId = -1;
//        } else {
//            expenseId = extras.getInt(EXPENSEINDEX);
//        }
//
//        if (expenseId >= 0)
//            expense = ApiClient.getExpense(expenseId);
//        else
//            expense = new Expense();
//
//        EditText editStore = (EditText) findViewById(R.id.edit_store);
//        editStore.setText(expense.getStore());
//
//        EditText editDesc = (EditText) findViewById(R.id.edit_desc);
//        editDesc.setText(expense.getDescription());
//
//        EditText editTitle = (EditText) findViewById(R.id.edit_title);
//        editTitle.setText(expense.getTitle());
//
//        EditText editAmount = (EditText) findViewById(R.id.edit_amount);
//        editAmount.setText(String.format("%.2f", expense.getAmount()));
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            // Respond to the action bar's Up/Home button
            case android.R.id.home:
                Intent upIntent = NavUtils.getParentActivityIntent(this);
                NavUtils.navigateUpTo(this, upIntent);
                return true;
        }
        return super.onOptionsItemSelected(item);
    }
}
