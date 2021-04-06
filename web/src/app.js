"use strict";

import $ from "/jquery.js";
import * as Common from "/common.js";
import * as Util from "/util.js";

import moment from "moment/src/moment.js";
import "moment/src/locale/de";
moment.locale("de");

const pages = {
  accounts: import("/pages/accounts/accounts.js"),
  expenses: import("/pages/expenses/expenses.js"),
  categories: import("/pages/categories/categories.js"),
  balances: import("/pages/balances/balances.js"),
  automation: import("/pages/automation/automation.js"),
  periodic: import("/pages/periodic/periodic.js"),
  devices: import("/pages/devices/devices.js"),
  analysis: import("/pages/analysis/analysis.js"),
  // stocks: import('/pages/stocks/stocks.js'),
  // 'stock-transactions': import('/pages/stock_transactions/stock_transactions.js'),
  // 'securities-accounts': import('/pages/securities_accounts/securities_accounts.js')
};

let currentTab = "";

function loadTab(e) {
  if (e in pages) {
    currentTab = e;

    $(".active", "#menu-main").each((x, y) => {
      if ($(y).attr("data-tab") != e)
        $(y).removeClass("active").removeClass("selected");
    });

    pages[e].then(function (x) {
      if (typeof x.location == "undefined") x.init("#page-" + e);
      else x.refresh();
    });
  }
}

$(document).ready(function () {
  Common.refresh().then(
    function () {
      $("#label-user").html('<i class="icon user"></i> ' + Common.me.fullName);

      $("#link-bundler").attr("href", "/report.html");

      $(".ui.dropdown").dropdown();
      $(".ui.checkbox").checkbox();

      $("#menu-main .item").tab({
        onVisible: function (e) {
          // only change the address if the address does not contain more of the same information
          let paths = $.address.path().substr(1).split("/");
          if (paths.length == 0 || paths[0] != e) $.address.path(e);

          if (currentTab == "accounts" || currentTab == "categories")
            Common.refresh().then(() => loadTab(e));
          else loadTab(e);
        },
      });

      let path = $.address.path().substr(1).split("/")[0];
      if (!(path in pages)) path = "analysis";
      $("#menu-main .item").tab("change tab", path);

      $.address.externalChange(function (event) {
        let p = event.path.substr(1).split("/")[0];
        if (!(p in pages)) p = "analysis";
        $("#menu-main .item").tab("change tab", p);
      });
    },
    function (e) {
      Util.errorMsg("Grundlegende Anfragen an den Server schlugen fehl.", e);
    }
  );
});
