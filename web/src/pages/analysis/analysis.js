"use strict";

import moment from "moment/src/moment.js";
import page from "./analysis.html";
import $ from "/jquery.js";
import _ from "lodash";
import * as Util from "/util.js";
import * as Api from "/moneydb.js";
import * as Sunburst from "./sunburst.js";

export let location;
export let currentTab = "analysis/computed_balances";

let overviewPromise;

const pages = {
  computed_balances: import("/pages/analysis/computed_balances.js"),
  monthly_overviews: import("/pages/analysis/monthly_overviews.js"),
  prepayments: import("/pages/analysis/prepayments.js"),
  history: import("/pages/analysis/history.js"),
  categories: import("/pages/analysis/categories.js"),
  statements: import("/pages/analysis/statements.js"),
  calendar: import("/pages/analysis/calendar.js"),
  // stock_portfolio: import('/pages/analysis/stock_portfolio.js'),
  // stock_portfolio_historic: import('/pages/analysis/stock_portfolio_historic.js')
};

let refreshNeeded = _.mapValues(pages, () => true);

let loadTab = function (e) {
  let p = e.split("/");
  if (p.length < 2 || p[0] != "analysis" || !(p[1] in pages)) return; // ignore wrong stuff

  // shorthand to just the subpage title
  currentTab = p[0] + "/" + p[1]; // ignore subtabs;
  p = p[1];

  if (!(p in pages)) return;

  switch (p) {
    case "computed_balances":
      pages[p].then(function (x) {
        if (typeof x.location == "undefined")
          x.init($("#computed-balances", location));
        else if (refreshNeeded[p]) x.refresh();

        refreshNeeded[p] = false;
      });

      break;
    case "calendar":
      pages[p].then(function (x) {
        if (typeof x.location == "undefined") x.init($("#calendar", location));
        else if (refreshNeeded[p]) x.refresh();

        refreshNeeded[p] = false;
      });

      break;
    case "statements":
      pages[p].then(function (x) {
        if (typeof x.location == "undefined")
          x.init($("#statements", location));
        else if (refreshNeeded[p]) x.refresh();

        refreshNeeded[p] = false;
      });

      break;
    // case 'stock_portfolio':
    //   pages[p].then(function(x) {
    //     if (typeof x.location == 'undefined')
    //       x.init($('#stock-portfolio', location));
    //     else if (refreshNeeded[p])
    //       x.refresh();

    //     refreshNeeded[p] = false;
    //   });

    //   break;
    // case 'stock_portfolio_historic':
    //   pages[p].then(function(x) {
    //     if (typeof x.location == 'undefined')
    //       x.init($('#stock-portfolio-historic', location));
    //     else if (refreshNeeded[p])
    //       x.refresh();

    //     refreshNeeded[p] = false;
    //   });

    //   break;
    case "prepayments":
      pages[p].then(function (x) {
        if (typeof x.location == "undefined")
          x.init($("#prepayments", location));
        else if (refreshNeeded[p]) x.refresh();

        refreshNeeded[p] = false;
      });

      break;
    case "history":
      $.when(pages[p], overviewPromise).then(function (x, json) {
        if (typeof x.location == "undefined")
          x.init($("#availability-chart", location));

        if (refreshNeeded[p]) x.refresh(json.monthly);

        refreshNeeded[p] = false;
      });
      break;
    case "categories":
      $.when(pages[p], overviewPromise).then(function (x, json) {
        if (typeof x.location == "undefined")
          x.init($("#category-chart", location));

        if (refreshNeeded[p]) {
          x.refresh(json.monthly);

          Sunburst.createBig(
            $("#allexpsbycat", location),
            Sunburst.categoryData(json.statsExps, "Ausgaben")
          );
          Sunburst.createBig(
            $("#allincomebycat", location),
            Sunburst.categoryData(json.statsIncome, "Einnahmen")
          );
        }

        refreshNeeded[p] = false;
      });

      break;
    case "monthly_overviews":
      $.when(pages[p], overviewPromise).then(function (x, json) {
        if (typeof x.location == "undefined")
          x.init($("#table-overviews", location));

        if (refreshNeeded[p]) x.refresh(json.monthly);

        refreshNeeded[p] = false;
      });
      break;
  }
};

export function init(loc) {
  location = $(loc);
  location.empty().html(page);

  $("#menu .item", location).tab({
    onVisible: function (e) {
      $.address.value(e);
      loadTab(e);
    },
  });

  refresh();
}

export function refresh() {
  overviewPromise = Api.getComputeOverviews(10).then(function (json) {
    json.monthly = _.map(json.monthly, function (x) {
      x.timespan =
        moment(x.startDate).format("L") +
        " bis " +
        moment(x.endDate).format("L");
      x.moneyStart =
        x.startBalances.total[x.startBalances.total.length - 1].amount;
      x.moneyEnd = x.endBalances.total[x.endBalances.total.length - 1].amount;
      x.moneyEndApprox =
        x.endBalancesApprox.total[x.endBalancesApprox.total.length - 1].amount;

      x.moneyDelta = Util.nullToNaN(x.moneyEnd) - Util.nullToNaN(x.moneyStart);
      x.moneyError = Util.nullToNaN(
        Math.abs(Util.nullToNaN(x.moneyEnd) - Util.nullToNaN(x.moneyEndApprox))
      );

      return x;
    });

    return json;
  });

  let paths = $.address.value().substr(1).split("/");
  let path = currentTab;

  if (paths.length >= 2) path = paths[0] + "/" + paths[1];
  else $.address.value(path);

  refreshNeeded = _.mapValues(refreshNeeded, () => true);
  $("#menu .item", location).tab("change tab", path);
  loadTab(path); // (the previous line might not fire the callback)

  $.address.externalChange(function (event) {
    let p = event.value.substr(1).split("/");
    if (p.length >= 2 && p[0] == "analysis") {
      if (!(p[1] in pages)) p[1] = "analysis";

      $("#menu .item").tab("change tab", p[0] + "/" + p[1]);
    }
  });
}
