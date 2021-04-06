'use strict';

import Table from '/table.js';
import moment from 'moment/src/moment.js';
import tinycolor from 'tinycolor2';
import $ from '/jquery.js';
import * as Util from '/util.js';
import * as Api from '/moneydb.js';
import * as Common from '/common.js';
import * as ExpenseModal from './expenses_modal.js';

export let table;
export let location;
export let dataTable;
export let queryOptions = {
  accounts: [],
  categories: [],
  from: null,
  to: null
};

let initAccountFilter = function() {
  let pr = $('#exptable_accs', location).parent();
  let prm = pr.find('.menu').empty();

  for (let j = 0; j < Common.visibleAccounts.length; j++) {
    let a = Common.visibleAccounts[j];

    // if (a.ownerId != user.id)
    prm.append('<div class="item" data-value="' + a.id + '">' + '<div class="color-box" style="background-color: ' + a.color + ';"></div>&nbsp;' + Common.accountToString(a) + '</div>');
  }

  $('#exptable_accs', location).parent().dropdown('refresh');
  $('#exptable_accs', location).change(function() {
    let as = $(this).val();
    if (as.length == 0 || as == '')
      queryOptions.accounts = [];
    else
      queryOptions.accounts = as.split(',').map(v => parseInt(v));

    dataTable.draw();
  });
};

let initCategoryFilter = function() {
  let pr = $('#exptable_cats', location).parent();
  let prm = pr.find('.menu').empty();

  for (let i = 0; i < Common.visibleCategories.length; i++) {
    let a = Common.visibleCategories[i];

    // if (a.ownerId != user.id)
    prm.append('<div class="item" data-value="' + a.id + '">' + '<div class="color-box" style="background-color: ' + a.color + ';"></div>&nbsp;' + Common.categoryToString(a) + '</div>');
  }

  $('#exptable_cats', location).parent().dropdown('refresh');
  $('#exptable_cats', location).change(function() {
    let cs = $(this).val();
    if (cs.length == 0 || cs == '')
      queryOptions.categories = [];
    else
      queryOptions.categories = cs.split(',').map(v => parseInt(v));

    dataTable.draw();
  });
};

export function init(loc) {
  location = $(loc);

  let opts = {
    location: location,
    url: 'expenses/rendered',
    defaultOrder: [
      [9, 'desc'],
      [1, 'desc'],
      [0, 'desc']
    ],
    expandable: true,
    formatter: function(data) {
      let s = '<div class="ui grid"><div class="row">';

      s += '<div class="six wide column">';
      s += '<div class="ui relaxed divided list" style="white-space: normal;">';

      s += '<div class="item"><div class="content"><div class="header">Titel</div>' +
        '<div class="description">' + data.title + '</div></div></div>';

      s += '<div class="item"><div class="content"><div class="header">Wertstellungsdatum</div>' +
        '<div class="description">' + moment(data.valueDate).format('LLLL') + '</div></div></div>';

      if (data.bookingDate) {
        s += '<div class="item"><div class="content"><div class="header">Buchungsdatum</div>' +
          '<div class="description">' + moment(data.bookingDate).format('LLLL') + '</div></div></div>';
      }

      if (data.description != '') {
        s += '<div class="item"><div class="content"><div class="header">Beschreibung</div>' +
          '<div class="description">' + data.description + '</div></div></div>';
      }

      if (data.transaction != '') {
        s += '<div class="item"><div class="content"><div class="header">Transaktion</div>' +
          '<div class="description"><p style="font-family:monospace">' + data.transaction.split('\n').join('<br />') + '</p></div></div></div>';
      }

      if (data.flags.length > 0) {
        s += '<div class="item"><div class="content"><div class="header">Flags</div>' +
          '<div class="description">' + data.flags.join(', ') + '</div></div></div>';
      }

      if (data.comments != '') {
        s += '<div class="item"><div class="content"><div class="header">Kommentare</div>' +
          '<div class="description">' + data.comments + '</div></div></div>';
      }

      s += '<div class="item"><div class="content"><div class="header">Erstellt</div>' +
        '<div class="description">' + moment(data.creationDate).format('LLLL') + '</div></div></div>';

      s += '<div class="item"><div class="content"><div class="header">Zuletzt geändert</div>' +
        '<div class="description">' + moment(data.lastModified).format('LLLL') + ' von ' + Common.findUser(data.lastModifiedBy).fullName + ' mit ' + data.lastModifiedThrough + '</div></div></div>';

      s += '</div></div>';

      if (data.sharing.length > 0) {
        s += `<div class="eight wide centered column"><table class="ui basic table">
        <thead>
          <tr>
            <th>Konto</th>
            <th>Betrag</th>
            <th>Grund</th>
          </tr>
        </thead>
        <tbody>`;

        let xs = [{
          account: data.account,
          amount: data.amount,
          reason: 'Haupttransaktion'
        }];

        data.sharing.forEach(function(x) {
          let reason;

          switch (x.sharingType) {
            case 'Equal':
              if (x.parameter == 1) {
                reason = 'Anteilig';
              } else reason = 'Anteilig mit Faktor ' + x.parameter.toFixed(2);
              break;
            case 'FixedAmount':
              reason = 'Fester Betrag von ' + Util.formatAmount(x.parameter);
              break;
            case 'FixedFraction':
              reason = 'Fester Anteil von ' + (100 * x.parameter).toFixed(0) + '%';
              break;
          }

          if (!reason) reason = x.sharingType + ', parameter = ' + x.parameter;

          xs.push({
            account: Common.findAccount(x.sharingAccountId),
            amount: x.calculatedAmount,
            reason: reason
          });
        });

        xs.forEach(function(x) {
          let c = tinycolor(x.account.color);
          let accStyle = 'background-color:' + c.toString() + ';';

          if (c.getBrightness() < 100) {
            accStyle += 'color:#F7F7F7;';
          }

          let amountStyle = '';
          let amountClass = '';

          if (x.account.ownerId == Common.me.id && x.amount != 0) {
            amountStyle = 'background-color:' + tinycolor(x.amount <= 0 ? '#FCFFF5' : '#FFF6F6').desaturate(10).darken(5).toString() + '!important;';
            amountClass = x.amount <= 0 ? 'positive' : 'negative';
          }

          s += '<tr><td style="' + accStyle + '">' + Common.accountToString(x.account) + '</td>' + '<td class="' + amountClass + '" style="' + amountStyle + '">' + Util.formatAmount(x.amount) + '</td><td>' + x.reason + '</td></tr>';
        });

        s += '</tbody><tfoot></tfoot></table></div>';
      }

      s += '</div></div>';
      return s;
    },
    isReadOnly: () => false,
    isDeletable: () => true,
    colorRow: function(rowjq, data) {
      rowjq.find('td:nth-child(10)').attr('style', '');
      rowjq.find('td:nth-child(6)').attr('style', '');

      if (data.sharing.length == 0) {
        Util.setBackgroundColor(rowjq.find('td:nth-child(4)'), data.account.color, false);
      } else {
        let colors = [data.account.color];
        let w1 = Math.abs(data.amount);
        if (w1 == 0) w1 = 1;
        let weights = [w1];
        let sums = [0, w1];

        for (let j = 0; j < data.sharing.length; j++) {
          let w = Math.abs(data.sharing[j].calculatedAmount);
          if (w == 0) w = 1;

          colors.push(data.sharing[j].account.color);
          weights.push(w);
          sums.push(sums[sums.length - 1] + weights[weights.length - 1]);
        }

        let totalWeight = weights.reduce((a, b) => a + b, 0);
        let s = 'border-style: none; background-image: linear-gradient(to right, ';

        for (let i = 0; i < colors.length; i++) {
          if (i != 0) s += ',';
          s += colors[i];

          if (i != 0) {
            s += ' ' + (sums[i] / totalWeight * 100).toFixed(0) + '%';
          }

          if (i != colors.length - 1) {
            s += ', ' + colors[i] + ' ' + (sums[i + 1] / totalWeight * 100).toFixed(0) + '%';
          }
        }
        s += ');';

        rowjq.find('td:nth-child(4)').attr('style', s);
      }
      Util.setBackgroundColor(rowjq.find('td:nth-child(5)'), data.category.color, false);
    },
    colorRowDark: function(row, data) {
      if (data.checked) {
        $(row).find('td:nth-child(10)').attr('style', 'background-color:' + tinycolor('#FCFFF5').desaturate(10).darken(5).toString() + '!important');
      } else if (data.flags.indexOf('Preliminary') == -1) {
        $(row).find('td:nth-child(10)').attr('style', 'background-color:' + tinycolor('#FFF6F6').desaturate(10).darken(5).toString() + '!important');
      }

      if (data.effectiveAmount != 0) {
        $(row).find('td:nth-child(6)').attr('style', 'background-color:' + tinycolor(data.effectiveAmount <= 0 ? '#FCFFF5' : '#FFF6F6').desaturate(10).darken(5).toString() + '!important');
      }

      // if (row.sharing.length == 0)
      // setTdColor(rowjq.find("td:nth-child(4)"), tinycolor(data.account.color), true);
      // setTdColor(rowjq.find("td:nth-child(5)"), tinycolor(data.category.color), true);
    },
    createdRow: function(row, data) {
      if (data.effectiveAmount != 0) {
        $(row).find('td:nth-child(6)').addClass(data.effectiveAmount <= 0 ? 'positive' : 'negative');
      }

      if (data.checked) {
        $(row).find('td:nth-child(10)').addClass('positive').html('<i class="checkmark icon"></i> Ja');
      } else if (data.flags.indexOf('Preliminary') != -1) {
        $(row).find('td:nth-child(10)').addClass('warning').html('<i class="attention icon"></i> Nein');
      } else {
        $(row).find('td:nth-child(10)').addClass('error').html('<i class="attention icon"></i> Nein');
      }

      // if (data.flags.indexOf('Preliminary') != -1) {
      //   $(row).find('td:nth-child(3)').css('color', '#333333');
      // }
    },
    preprocess: function(row) {
      if ((row.flags.indexOf('NeedsAttention') != -1 && row.flags.indexOf('Template') == -1) || row.flags.indexOf('Preliminary') != -1) {
        row.checked = false;
      } else if (row.flags.indexOf('Preliminary') != -1) {
        $(row).find('td:nth-child(10)').addClass('warning').html('<i class="attention icon"></i> Vorläufig');
      } else {
        row.checked = true;
      }

      row.account = Common.findAccount(row.accountId);
      row.category = Common.findCategory(row.categoryId);

      for (let i = 0; i < row.sharing.length; i++) {
        row.sharing[i].account = Common.findAccount(row.sharing[i].sharingAccountId);
      }

      return row;
    },
    extraQueryData: () => queryOptions,
    columns: [{
      title: 'ID',
      data: 'id',
      width: '2.25em'
    }, {
      title: 'Datum',
      data: 'valueDate',
      width: '4.5em',
      render: function(data, type, row) {
        return moment(row.valueDate).format('L');
      }
    }, {
      title: 'Titel',
      data: 'title',
      width: '15em',
      render: function(data, type, row) {
        if (row.flags.indexOf('Preliminary') != -1)
          return '<span style="color:#999999">(vorläufig)</span> ' + data;
        else return data;
      }
    }, {
      title: 'Konto',
      data: 'accountId',
      width: '11em',
      render: function(data, type, row) {
        if (row.sharing.length == 0) {
          return Common.accountToString(row.account);
        } else {
          let s = Common.accountToString(row.account);
          let aids = [row.accountId];

          for (let i = 0; i < row.sharing.length; i++) {
            if (!aids.includes(row.sharing[i].sharingAccountId)) {
              s += ' + ' + Common.accountToString(Common.findAccount(row.sharing[i].sharingAccountId));
              aids.push(row.sharing[i].sharingAccountId);
            }
          }

          return s;
        }
      }
    }, {
      title: 'Kategorie',
      data: 'categoryId',
      width: '8em',
      render: function(data, type, row) {
        return Common.categoryToString(row.category);
      }
    }, {
      title: 'Betrag',
      data: 'effectiveAmount',
      width: '4.5em',
      orderable: false,
      render: function(data, type, row) {
        return Util.formatAmount(row.effectiveAmount);
      }
    }, {
      title: 'Gesch&auml;ft',
      data: 'store',
      width: '8em'
    }, {
      title: 'Beschreibung',
      data: 'description'
    }, {
      title: 'Zus&auml;tzliche Angaben',
      data: 'transaction'
    }, {
      title: 'Überprüft',
      data: 'checked',
      width: '4.5em'
    }],
    removeHandler: function(selectedExpense, draw) {
      $('#modal-delete p').html('Ausgabe <b>' + selectedExpense.id + '</b> (<b>' + selectedExpense.title + '</b> am ' + moment(selectedExpense.valueDate).format('LLLL') + ') wirklich löschen? Dies kann nicht rückgängig gemacht werden!');
      $('#modal-delete').modal({
        closable: false,
        onApprove: function() {
          Api.deleteExpensesRenderedById(selectedExpense.id, function() {
            draw();
          }, function(e) {
            Util.errorMsg('Konnte Ausgabe nicht löschen.', e);
          });
        }
      }).modal('show');
    },
    addHandler: function(draw) {
      $('#modal-expenses .header').html('Ausgabe erstellen');
      ExpenseModal.reset();

      $('#modal-expenses').modal({
        observeChanges: true,
        allowMultiple: true,
        closable: false,
        onApprove: function() {
          $('#modal-expenses .ui.form').submit();
          if (!$('#modal-expenses .ui.form').form('is valid')) {
            return false;
          }

          Api.postExpensesRendered(ExpenseModal.save(undefined), function() {
            draw();
          }, function(e) {
            Util.errorMsg('Konnte Ausgabe nicht erstellen.', e);
          });

          draw();
        },
        onVisible: function() {
          ExpenseModal.reset();
        },
        onHidden: function() {
          $('#modal-expenses-sharing').modal('hide');
        }
      }).modal('show');
    },
    modifyHandler: function(selectedExpense, draw) {
      $('#modal-expenses .header').html('Ausgabe bearbeiten');
      ExpenseModal.load(selectedExpense);

      $('#modal-expenses').modal({
        observeChanges: true,
        allowMultiple: true,
        closable: false,
        onApprove: function() {
          $('#modal-expenses .ui.form').submit();
          if (!$('#modal-expenses .ui.form').form('is valid')) {
            return false;
          }

          Api.putExpensesRenderedById(selectedExpense.id, ExpenseModal.save(selectedExpense), function() {
            draw();
          }, function(e) {
            Util.errorMsg('Konnte Ausgabe nicht bearbeiten.', e);
          });

          draw();
        },
        onVisible: function() {
          ExpenseModal.load(selectedExpense);
        },
        onHidden: function() {
          $('#modal-expenses-sharing').modal('hide');
        }
      }).modal('show');
    },
    duplicateHandler: function(selectedExpense, draw) {
      $('#modal-expenses .header').html('Ausgabe kopieren');
      ExpenseModal.load(selectedExpense);

      $('#modal-expenses').modal({
        observeChanges: true,
        allowMultiple: true,
        closable: false,
        onApprove: function() {
          $('#modal-expenses .ui.form').submit();
          if (!$('#modal-expenses .ui.form').form('is valid')) {
            return false;
          }

          Api.postExpensesRendered(ExpenseModal.save(undefined), function() {
            draw();
          }, function(e) {
            Util.errorMsg('Konnte Ausgabe nicht erstellen.', e);
          });

          draw();
        },
        onVisible: function() {
          ExpenseModal.load(selectedExpense);
        },
        onHidden: function() {
          $('#modal-expenses-sharing').modal('hide');
        }
      }).modal('show');
    },
    extraHandler: function(selectedExpense, draw) {
      Api.postExpensesRecheckById(selectedExpense.id, function() {
        draw();
      }, function(e) {
        Util.errorMsg('Konnte Ausgabe nicht filtern.', e);
      });
    },
    extraButton: '<i class="icon calculator"></i> Einsortieren'
  };

  table = new Table(opts);
  table.init();
  dataTable = table.table;

  let filteringText = '<div class="ui form"><div class="fields">';
  filteringText += '<div class="three wide field">';
  filteringText += '<div class="ui calendar" id="exptable_from"><div class="ui input left icon">';
  filteringText += '<i class="calendar icon"></i><input type="text" placeholder="von" name="date">';
  filteringText += '</div></div></div>';

  filteringText += '<div class="three wide field">';
  filteringText += '<div class="ui calendar" id="exptable_to"><div class="ui input left icon">';
  filteringText += '<i class="calendar icon"></i><input type="text" placeholder="bis" name="date">';
  filteringText += '</div></div></div>';

  filteringText += '<div class="five wide field">';
  filteringText += '<div class="ui fluid selection dropdown multiple">';
  filteringText += '<input id="exptable_cats" type="hidden">';
  filteringText += '<i class="dropdown icon"></i><div class="default text">Kategorien</div>';
  filteringText += '<div class="menu"></div></div';
  filteringText += '</div></div></div>';

  filteringText += '<div class="five wide field">';
  filteringText += '<div class="ui fluid selection dropdown multiple">';
  filteringText += '<input id="exptable_accs" type="hidden">';
  filteringText += '<i class="dropdown icon"></i><div class="default text">Konten</div>';
  filteringText += '<div class="menu"></div></div';
  filteringText += '</div></div></div>';

  filteringText += '</div>';
  filteringText += '</div>';

  table.extraColumn.prepend(filteringText);

  $('#exptable_from', location).calendar({
    type: 'date',
    today: true,
    firstDayOfWeek: 1,
    formatter: {
      date: d => moment(d).format('L')
    },
    parser: {
      date: d => moment(d, 'L').toDate()
    },
    onChange: function(d) {
      let tmp = queryOptions.from;

      if (typeof d !== 'undefined') {
        // somehow, the time of the date calendar is not 00:00 ...
        let f = moment(moment(d).format('L'), 'L');
        queryOptions.from = f.toISOString();
      } else
        queryOptions.from = null;

      if (tmp != queryOptions.from)
        dataTable.draw();

      return true;
    }
  }).calendar('set date', null, true, false);

  $('#exptable_to', location).calendar({
    type: 'date',
    today: true,
    firstDayOfWeek: 1,
    formatter: {
      date: d => moment(d).format('L')
    },
    parser: {
      date: d => moment(d, 'L').toDate()
    },
    onChange: function(d) {
      let tmp = queryOptions.to;

      if (typeof d !== 'undefined') {
        // somehow, the time of the date calendar is not 00:00 ...
        let t = moment(moment(d).format('L'), 'L');
        queryOptions.to = t.toISOString();
      } else
        queryOptions.to = null;

      if (tmp != queryOptions.to)
        dataTable.draw();

      return true;
    }
  }).calendar('set date', null, true, false);

  initCategoryFilter();
  initAccountFilter();

  ExpenseModal.init();
}

export function refresh() {
  table.refresh();
}
