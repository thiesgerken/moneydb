'use strict';

import Table from '/table.js';
import moment from 'moment/src/moment.js';
import $ from '/jquery.js';
import * as Util from '/util.js';
import * as Api from '/moneydb.js';
import * as Common from '/common.js';
import * as BalanceModal from './balances_modal.js';

export let table;
export let location;
export let dataTable;
export let queryOptions = {
  accounts: [],
  categories: [],
  from: null,
  to: null
};

export function init(loc) {
  location = $(loc);

  let opts = {
    url: 'balances',
    location: location,
    defaultOrder: [
      [1, 'desc'],
      [0, 'desc']
    ],
    lengths: [25, 50],
    expandable: false,
    extraQueryData: () => queryOptions,
    formatter: function(data) {
      let s = '<div class="ui relaxed divided list">';

      // s += '<div class="item"><div class="content"><div class="header">Besitzer</div>' +
      //   '<div class="description">' + data.owner + '</div></div></div>';

      s += '<div class="item"><div class="content"><div class="header">Datum</div>' +
        '<div class="description">' + moment(data.date).format('LLLL') + '</div></div></div>';

      return s + '</div>';
    },
    isReadOnly: () => false,
    isDeletable: () => true,
    colorRow: function(row, data) {
      // rowjq.find("td:nth-child(3)").attr('style', '');

      Util.setBackgroundColor($(row).find('td:nth-child(4)'), data.account.color, false);
    },
    colorRowDark: function(row, data) {
      // rowjq.find("td:nth-child(3)").attr('style', 'background-color:' + tinycolor(data.amount >= 0 ? '#FCFFF5' : '#FFF6F6').desaturate(10).darken(5).toString() + '!important');

      Util.setBackgroundColor($(row).find('td:nth-child(4)'), data.account.color, true);
    },
    createdRow: function(row, data) {
      $(row).find('td:nth-child(3)').addClass(data.amount >= 0 ? 'positive' : 'negative');
      $(row).find('td').last().html('');
    },
    preprocess: function(row) {
      row.account = Common.findAccount(row.accountId);
      return row;
    },
    columns: [{
      data: 'id',
      title: 'ID',
      width: '2.5em'
    }, {
      data: 'date',
      title: 'Datum',
      width: '10em',
      render: function(data, type, row) {
        return moment(row.date).format('L LT');
      }
    }, {
      data: 'amount',
      title: 'Betrag',
      width: '5.5em',
      render: function(data, type, row) {
        return Util.formatAmount(row.amount);
      }
    }, {
      data: 'accountId',
      title: 'Konto',
      width: '14em',
      render: function(data, type, row) {
        return Common.accountToString(row.account);
      }
    }, {
      data: 'account',
      title: '',
      'orderable': false
    }],
    removeHandler: function(item, draw) {
      $('#modal-delete p').html('Kontostand <b>' + item.id + '</b> (<b>' + Util.formatAmount(item.amount) + '</b> auf <b>' + Common.accountToString(item.account) + '</b> am <b>' + moment(item.date).format('LLLL') + '</b>) wirklich löschen? Dies kann nicht rückgängig gemacht werden!');

      $('#modal-delete').modal({
        closable: false,
        onApprove: function() {
          Api.deleteBalancesById(item.id, function() {
            draw();
          }, function(e) {
            Util.errorMsg('Konnte Kontostand nicht löschen.', e);
          });
        }
      }).modal('show');
    },
    addHandler: function(draw) {
      $('#modal-balances .header').html('Kontostand erstellen');
      BalanceModal.reset();

      $('#modal-balances').modal({
        observeChanges: true,
        allowMultiple: true,
        closable: false,
        onApprove: function() {
          $('#modal-balances .ui.form').submit();
          if (!$('#modal-balances .ui.form').form('is valid'))
            return false;

          Api.postBalances(BalanceModal.save(undefined), function() {
            draw();
          }, function(e) {
            Util.errorMsg('Konnte Kontostand nicht erstellen.', e);
          });

          draw();
        },
        onVisible: function() {
          BalanceModal.reset();
        }
      }).modal('show');
    },
    modifyHandler: function(selectedBalance, draw) {
      $('#modal-balances .header').html('Kontostand bearbeiten');
      BalanceModal.load(selectedBalance);

      $('#modal-balances').modal({
        observeChanges: true,
        allowMultiple: true,
        closable: false,
        onApprove: function() {
          $('#modal-balances .ui.form').submit();
          if (!$('#modal-balances .ui.form').form('is valid'))
            return false;

          Api.putBalancesById(selectedBalance.id, BalanceModal.save(selectedBalance), function() {
            draw();
          }, function(e) {
            Util.errorMsg('Konnte Kontostand nicht bearbeiten.', e);
          });

          draw();
        },
        onVisible: function() {
          BalanceModal.load(selectedBalance);
        }
      }).modal('show');
    },
    duplicateHandler: function(selectedBalance, draw) {
      $('#modal-balances .header').html('Kontostand kopieren');
      BalanceModal.load(selectedBalance);

      $('#modal-balances').modal({
        observeChanges: true,
        allowMultiple: true,
        closable: false,
        onApprove: function() {
          $('#modal-balances .ui.form').submit();
          if (!$('#modal-balances .ui.form').form('is valid'))
            return false;

          Api.postBalances(BalanceModal.save(undefined), function() {
            draw();
          }, function(e) {
            Util.errorMsg('Konnte Kontostand nicht erstellen.', e);
          });

          draw();
        },
        onVisible: function() {
          BalanceModal.load(selectedBalance);
        }
      }).modal('show');
    }
  };

  table = new Table(opts);
  table.init();
  dataTable = table.table;

  BalanceModal.init();
}

export function refresh() {
  table.refresh();
}
