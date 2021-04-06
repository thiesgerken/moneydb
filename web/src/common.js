"use strict";

import $ from "/jquery.js";
import _ from "lodash";
import * as Api from "/moneydb.js";
import * as Util from "/util.js";

export let me;
export let visibleCategories = [];
export let visibleUsers = [];
export let visibleAccounts = [];

let securitiesAccounts = null;
let stocks = null;

export function getStocks() {
  let p = $.Deferred();

  if (stocks === null)
    refresh_other().then(function () {
      p.resolve(stocks);
    });
  else p.resolve(stocks);

  return p;
}

export function getSecuritiesAccounts() {
  let p = $.Deferred();

  if (securitiesAccounts === null)
    refresh_other().then(function () {
      p.resolve(securitiesAccounts);
    });
  else p.resolve(securitiesAccounts);

  return p;
}

export function refresh_other() {
  let p = $.Deferred();

  $.when(Api.getStocksAccounts(0, 0), Api.getStocksRendered(0, 0)).then(
    function (sas, ss) {
      securitiesAccounts = sortByOwner(_.map(sas[0], Util.flattenRecord));
      stocks = sortByOwner(
        _.map(ss[0], (row) => {
          row = Util.flattenRecord(row);

          if (row.info == null)
            row.info = {
              id: -1,
              title: "",
              kind: "",
              managingCompany: "",
              onvistaUrl: "",
              wkn: "",
            };
          else row.info = Util.flattenRecord(row.info);

          row.exchanges = _.map(row.exchanges, (e) => {
            e.data.firstRecord = Util.flattenRecord(e.data.firstRecord);
            e.data.lastRecord = Util.flattenRecord(e.data.lastRecord);

            return Util.flattenRecord(e);
          });
          return row;
        })
      );

      p.resolve();
    }
  );

  return p;
}

export function refresh() {
  let p = $.Deferred();

  $.when(Api.getUsersMe()).then(function (u) {
    $.when(
      Api.getAccounts(0, 0),
      Api.getCategories(0, 0),
      Api.getUsers(0, 0)
    ).then(function (as, cs, us) {
      me = Util.flattenRecord(u);
      visibleAccounts = sortByOwner(_.map(as[0], Util.flattenRecord));
      visibleCategories = sortByOwner(_.map(cs[0], Util.flattenRecord));
      visibleUsers = _.map(us[0], Util.flattenRecord);

      p.resolve();
    });
  });

  return p;
}

export function findUser(uid) {
  let x = _.find(visibleUsers, (u) => u.id == uid);

  if (typeof x == "undefined") return Util.unknownUser;
  else return x;
}

export function findAccount(aid) {
  let x = _.find(visibleAccounts, (a) => a.id == aid);

  if (typeof x == "undefined") return Util.unknownAccount;
  else return x;
}

export function findSecuritiesAccount(aid) {
  let x = _.find(securitiesAccounts, (a) => a.id == aid);

  if (typeof x == "undefined") return {};
  else return x;
}

export function findStock(aid) {
  let x = _.find(stocks, (a) => a.id == aid);

  if (typeof x == "undefined") return {};
  else return x;
}

export function findCategory(cid) {
  let x = _.find(visibleCategories, (c) => c.id == cid);

  if (typeof x == "undefined") return Util.unknownCategory;
  else return x;
}

export function sortByOwner(xs) {
  return _.concat(
    _.filter(xs, (x) => x.ownerId == me.id),
    _.filter(xs, (x) => x.ownerId != me.id)
  );
}

export function accountToString(a) {
  if (a.ownerId != me.id) return findUser(a.ownerId).fullName + " / " + a.title;
  else return a.title;
}

export function categoryToString(c) {
  if (c.ownerId != me.id) return findUser(c.ownerId).fullName + " / " + c.title;
  else return c.title;
}

export function makeAccountDropdown(location, all = true, allowEmpty = false) {
  let menu = $(location).parent().find(".menu").empty();

  if (allowEmpty)
    menu.append('<div class="item" data-value="-1">' + "&nbsp;</div>");

  _.forEach(visibleAccounts, function (a) {
    if (all || a.ownerId == me.id) {
      menu.append(
        '<div class="item" data-value="' +
          a.id +
          '">' +
          '<div class="color-box" style="background-color: ' +
          a.color +
          ';"></div>&nbsp;' +
          accountToString(a) +
          "</div>"
      );
    }
  });
}

export function makeSecuritiesAccountDropdown(location) {
  let menu = $(location).parent().find(".menu").empty();

  _.forEach(securitiesAccounts, function (a) {
    menu.append(
      '<div class="item" data-value="' + a.id + '">' + a.title + "</div>"
    );
  });
}

export function makeStockDropdown(location) {
  let menu = $(location).parent().find(".menu").empty();

  _.forEach(stocks, function (a) {
    let tit = a.info.title;
    if (tit.length > 50) tit = tit.substr(0, 46) + " ...";

    menu.append(
      '<div class="item" data-value="' +
        a.id +
        '">' +
        a.isin +
        "&nbsp;(" +
        a.info.title +
        ")</div>"
    );
  });
}

export function makeCategoryDropdown(location, all = true, allowEmpty = false) {
  let menu = $(location).parent().find(".menu").empty();

  if (allowEmpty)
    menu.append('<div class="item" data-value="-1">' + "&nbsp;</div>");

  _.forEach(visibleCategories, function (c) {
    if (all || c.ownerId == me.id) {
      menu.append(
        '<div class="item" data-value="' +
          c.id +
          '">' +
          '<div class="color-box" style="background-color: ' +
          c.color +
          ';"></div>&nbsp;' +
          categoryToString(c) +
          "</div>"
      );
    }
  });
}
