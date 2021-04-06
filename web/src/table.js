"use strict";

import $ from "/jquery.js";
import _ from "lodash";
import * as Util from "/util.js";

export default class Table {
  constructor(opts) {
    this.table = null;
    this.selectedItem = null;
    this.selectedRow = null;

    this.location = $(opts["location"]);
    this.formatter = opts["formatter"];
    this.colorRow = opts["colorRow"];
    this.colorRowDark = opts["colorRowDark"];
    this.isReadOnly = opts["isReadOnly"];
    this.isDeletable = opts["isDeletable"];
    this.url = opts["url"];
    this.columns = opts["columns"];
    this.createdRow = opts["createdRow"];
    this.defaultOrder = opts["defaultOrder"];
    this.addHandler = opts["addHandler"];
    this.removeHandler = opts["removeHandler"];
    this.modifyHandler = opts["modifyHandler"];
    this.refreshHandler = opts["refreshHandler"];
    this.duplicateHandler = opts["duplicateHandler"];
    this.expandable = opts["expandable"];

    this.preprocess = opts["preprocess"];
    if (!(typeof this.preprocess === "function"))
      this.preprocess = function (x) {
        return x;
      };

    this.extraQueryData = opts["extraQueryData"];
    if (!(typeof this.extraQueryData === "function"))
      this.extraQueryData = function () {
        return "";
      };

    this.lengths = "lengths" in opts ? opts.lengths : [15, 25, 50];

    this.showCopyButton = "showCopyButton" in opts ? opts.showCopyButton : true;
    this.showEditButton = "showEditButton" in opts ? opts.showEditButton : true;
    this.showNewButton = "showNewButton" in opts ? opts.showNewButton : true;
    this.showDeleteButton =
      "showDeleteButton" in opts ? opts.showDeleteButton : true;
    this.showRefreshButton =
      "showRefreshButton" in opts ? opts.showRefreshButton : false;

    if ("extraButton" in opts) {
      this.extraButton = opts["extraButton"];
      this.extraHandler = opts["extraHandler"];
    }

    if ("data" in opts) this.data = opts.data;
    else this.data = null;
  }

  selectItem(rowjq) {
    this.unselectItem();

    let row = this.table.row(rowjq);

    this.colorRowDark(rowjq, row.data());

    rowjq.addClass("active");
    this.selectedItem = row.data();
    this.selectedRow = rowjq;

    if (
      (typeof this.expandable === "function" && this.expandable(row.data())) ||
      (typeof this.expandable == "boolean" && this.expandable)
    ) {
      row.child(this.formatter(row.data()), "active").show();
      rowjq.addClass("shown");
    }

    if (this.isDeletable(row.data())) {
      $("#buttonRemove", this.location).prop("disabled", false);
      $("#buttonRemove", this.location).removeClass("disabled");
    }

    if (!this.isReadOnly(row.data())) {
      $("#buttonModify", this.location).prop("disabled", false);
      $("#buttonModify", this.location).removeClass("disabled");
      $("#buttonClone", this.location).prop("disabled", false);
      $("#buttonClone", this.location).removeClass("disabled");
      $("#buttonExtra", this.location).prop("disabled", false);
      $("#buttonExtra", this.location).removeClass("disabled");
    }
  }

  unselectItem() {
    if (this.selectedRow != null)
      this.colorRow(this.selectedRow, this.selectedItem);

    this.selectedItem = null;
    this.selectedRow = null;
    this.table.$("tr.active").removeClass("active");

    $("#buttonRemove", this.location).prop("disabled", true);
    $("#buttonRemove", this.location).addClass("disabled");
    $("#buttonModify", this.location).prop("disabled", true);
    $("#buttonModify", this.location).addClass("disabled");
    $("#buttonClone", this.location).prop("disabled", true);
    $("#buttonClone", this.location).addClass("disabled");
    $("#buttonExtra", this.location).prop("disabled", true);
    $("#buttonExtra", this.location).addClass("disabled");

    let me = this;

    this.table.$("tr.shown").each(function () {
      $(this).removeClass("shown");
      me.table.row($(this)).child.hide();
    });
  }

  refresh() {
    this.table.draw();
  }

  init() {
    let me = this;

    let dtopts = {
      processing: true,
      lengthMenu: me.lengths,
      pagingType: "full_numbers",
      autoWidth: false,
      order: _.cloneDeep(this.defaultOrder),
      createdRow: function (row, data) {
        me.colorRow($(row), data);
        me.createdRow(row, data);
      },
      language: Util.datatableLanguage,
      columns: this.columns,
    };

    if (this.data != null) dtopts.data = this.data;
    else {
      dtopts.serverSide = true;
      dtopts.ajax = {
        url: "api/" + this.url + "/query",
        data: function (d) {
          d.extra = me.extraQueryData();

          return JSON.stringify(d);
        },
        type: "POST",
        contentType: "application/json",
        dataSrc: function (json) {
          return _.map(json.data, (x) => me.preprocess(Util.flattenRecord(x)));
        },
      };
    }

    this.table = $(".table", this.location).DataTable(dtopts);

    $("tbody", this.location).on("click", "tr", function () {
      if ($(this).hasClass("even") || $(this).hasClass("odd")) {
        if ($(this).hasClass("active")) {
          me.unselectItem();
        } else {
          me.selectItem($(this));
        }
      }
    });

    $(this.location).on("draw.dt", function () {
      me.unselectItem();

      // let info = me.table.page.info();
      // if (info.page != 0)
      //   $.address.parameter('page', info.page + 1);
      // else
      //   $.address.parameter('page', '');
      //
      // if (info.length != me.lengths[0])
      //   $.address.parameter('length', info.length);
      // else
      //   $.address.parameter('length', '');
      //
      // if (me.table.search() != '')
      //   $.address.parameter('search', me.table.search());
      // else
      //   $.address.parameter('search', '');
      //
      // if (!_.isEqual(me.table.order(), me.defaultOrder))
      //   $.address.parameter('order', JSON.stringify(me.table.order()));
      // else
      //   $.address.parameter('order', '');
    });

    let rws = $(".grid .row", this.table.table().container());
    rws
      .children(".column")
      .first()
      .detach()
      .prependTo($(".grid .row", this.table.table().container()).last());

    $("input", rws.children(".column").first()).attr("placeholder", "Suche");
    rws
      .children(".column")
      .first()
      .removeClass("eight")
      .removeClass("wide")
      .addClass("two")
      .addClass("wide");

    let btns = '<div class="eight wide column"></div>';
    rws.first().prepend(btns);
    this.extraColumn = $(".column", rws.first()).first();

    btns = '<div class="six wide column"><div class="ui buttons">';

    if (this.showRefreshButton)
      btns +=
        '<div class="ui small blue button" id="buttonRefresh"><i class="icon refresh"></i> Aktualisieren</div>';

    if (this.showNewButton)
      btns +=
        '<div class="ui small blue button" id="buttonAdd"><i class="icon add"></i> Neu</div>';

    if (this.showCopyButton)
      btns +=
        '<div class="ui small disabled teal button" id="buttonClone"><i class="icon copy"></i> Duplizieren</div>';

    if (this.showEditButton)
      btns +=
        '<div class="ui small disabled orange button" id="buttonModify"><i class="icon edit"></i> Bearbeiten</div>';

    if (this.showDeleteButton)
      btns +=
        '<div class="ui small disabled negative button" id="buttonRemove"><i class="icon delete"></i> Löschen</div>';

    if ("extraButton" in this)
      btns +=
        '<div class="ui small disabled button" id="buttonExtra">' +
        this.extraButton +
        "</div>";

    btns += "</div></div>";
    rws.first().prepend(btns);

    $(".input", rws.first())
      .first()
      .attr(
        "data-tooltip",
        'Wildcards: "%" (beliebig viele Zeichen) und "_" (einzelnes Zeichen)'
      );

    $("#buttonRemove", this.location).click(function () {
      me.removeHandler(me.selectedItem, () => me.table.draw(false));
    });

    $("#buttonModify", this.location).click(function () {
      me.modifyHandler(me.selectedItem, () => me.table.draw(false));
    });

    $("#buttonClone", this.location).click(function () {
      me.duplicateHandler(me.selectedItem, () => me.table.draw(false));
    });

    $("#buttonExtra", this.location).click(function () {
      me.extraHandler(me.selectedItem, () => me.table.draw(false));
    });

    $("#buttonAdd", this.location).click(function () {
      me.addHandler(() => me.table.draw(false));
    });

    $("#buttonRefresh", this.location).click(function () {
      me.refreshHandler(() => me.table.draw(false));
    });

    rws.last().children(".seven").attr("class", "four wide column");
    rws.last().children(".eight").attr("class", "three wide column");
    rws
      .last()
      .children()
      .first()
      .children()
      .attr(
        "style",
        "position: absolute;top: 50%;transform: translateY(-50%);"
      );

    // me.table.draw(false);
  }
}
