'use strict';

import Table from '/table.js';
import $ from '/jquery.js';
import * as Util from '/util.js';
import * as Api from '/moneydb.js';
import * as Common from '/common.js';
import * as StockTransactionModal from './stock_transactions_modal.js';
import moment from 'moment/src/moment.js';
import tinycolor from 'tinycolor2';

export let table;
export let location;
export let dataTable;

export function init(loc) {
  location = $(loc);

  let opts = {
    url: 'stocks/transactions',
    location: location,
    defaultOrder: [
      [1, 'desc'],
      [0, 'desc']
    ],
    lengths: [25, 50],
    expandable: true,
    formatter: function(data) {
      let s = '<div class="ui relaxed divided list">';

      s +=
        '<div class="item"><div class="content"><div class="header">Datum</div>' +
        '<div class="description">' + moment(data.date).format('LL LT') + '</div></div></div>';

      s +=
        '<div class="item"><div class="content"><div class="header">Handelsplatz</div>' +
        '<div class="description">' + data.exchange + '</div></div></div>';

      s +=
        '<div class="item"><div class="content"><div class="header">Gesamtbetrag</div>' +
        '<div class="description">' + Util.formatAmount(data.amount) + ' + ' + Util.formatAmount(data.fees) + ' Gebühren</div></div></div>';

      s += '</div>';
      return s;
    },
    isReadOnly: () => false,
    isDeletable: () => true,
    colorRow: (rowjq) => {
      rowjq.find('td:nth-child(3)').attr('style', '');
      rowjq.find('td:nth-child(5)').attr('style', '');
    },
    colorRowDark: function(row, data) {
      if (data.totalAmount != 0) {
        $(row).find('td:nth-child(3)').attr('style', 'background-color:' + tinycolor(data.totalAmount <= 0 ? '#FCFFF5' : '#FFF6F6').desaturate(10).darken(5).toString() + '!important');
      }

      if (data.units != 0) {
        $(row).find('td:nth-child(5)').attr('style', 'background-color:' + tinycolor(data.units >= 0 ? '#FCFFF5' : '#FFF6F6').desaturate(10).darken(5).toString() + '!important');
      }
    },
    createdRow: function(row, data) {
      if (data.totalAmount != 0) {
        $(row).find('td:nth-child(3)').addClass(data.totalAmount <= 0 ? 'positive' : 'negative');
      }

      if (data.units != 0) {
        $(row).find('td:nth-child(5)').addClass(data.units >= 0 ? 'positive' : 'negative');
      }
    },
    preprocess: function(row) {
      row.account = Common.findSecuritiesAccount(row.accountId);
      row.stock = Common.findStock(row.stockId);
      row.totalAmount = row.amount + row.fees;

      return row;
    },
    columns: [{
      data: 'id',
      title: 'ID',
      width: '2.5em'
    }, {
      title: 'Datum',
      data: 'date',
      width: '4.5em',
      render: function(data) {
        return moment(data).format('L');
      }
    }, {
      title: 'Gesamtbetrag',
      data: 'price',
      width: '7em',
      orderable: false,
      render: function(_data, _type, row) {
        return Util.formatAmount(row.totalAmount);
      }
    }, {
      title: 'Kurs',
      data: 'units',
      width: '5em',
      orderable: false,
      render: function(_data, _type, row) {
        return Util.formatAmount(row.amount / row.units);
      }
    }, {
      title: 'Einheiten',
      data: 'units',
      width: '6em',
      render: function(data) {
        return data.toFixed(4).replace('.', ',');
      }
    }, {
      title: 'ISIN',
      data: 'stockId',
      width: '9em',
      render: function(_data, _type, row) {
        return row.stock.isin;
      }
    }, {
      title: 'Wertpapier',
      data: 'stockId',
      render: function(_data, _type, row) {
        return row.stock.info.title;
      }
    }, {
      title: 'Depot',
      data: 'accountId',
      width: '11em',
      render: function(_data, _type, row) {
        return row.account.title;
      }
    }],
    removeHandler: function(item, draw) {
      $('#modal-delete p').html('Transaktion <b>' + item.id + '</b> (<b>' + item.title + '</b>) wirklich löschen? Dies kann nicht rückgängig gemacht werden!');
      $('#modal-delete').modal({
        closable: false,
        onApprove: function() {
          Api.deleteStocksTransactionsById(item.id, function() {
            draw();
          }, function(e) {
            Util.errorMsg('Konnte Transaktion nicht löschen.', e);
          });
        }
      }).modal('show');
    },
    addHandler: function(draw) {
      $('#modal-stock-transactions .header').html('Transaktion erstellen');
      StockTransactionModal.reset();

      $('#modal-stock-transactions').modal({
        observeChanges: true,
        allowMultiple: true,
        closable: false,
        onApprove: function() {
          $('#modal-stock-transactions .ui.form').submit();
          if (!$('#modal-stock-transactions .ui.form').form('is valid'))
            return false;

          Api.postStocksTransactions(StockTransactionModal.save(undefined), function() {
            draw();
          }, function(e) {
            Util.errorMsg('Konnte Transaktion nicht erstellen.', e);
          });

          draw();
        },
        onVisible: function() {
          StockTransactionModal.reset();
        }
      }).modal('show');
    },
    modifyHandler: function(selectedTransaction, draw) {
      $('#modal-stock-transactions .header').html('Transaktion bearbeiten');
      StockTransactionModal.load(selectedTransaction);

      $('#modal-stock-transactions').modal({
        observeChanges: true,
        allowMultiple: true,
        closable: false,
        onApprove: function() {
          $('#modal-stock-transactions .ui.form').submit();
          if (!$('#modal-stock-transactions .ui.form').form('is valid'))
            return false;

          Api.putStocksTransactionsById(selectedTransaction.id, StockTransactionModal.save(selectedTransaction), function() {
            draw();
          }, function(e) {
            Util.errorMsg('Konnte Transaktion nicht bearbeiten.', e);
          });

          draw();
        },
        onVisible: function() {
          StockTransactionModal.load(selectedTransaction);
        }
      }).modal('show');
    },
    duplicateHandler: function(selectedTransaction, draw) {
      $('#modal-stock-transactions .header').html('Transaktion kopieren');
      StockTransactionModal.load(selectedTransaction);

      $('#modal-stock-transactions').modal({
        observeChanges: true,
        allowMultiple: true,
        closable: false,
        onApprove: function() {
          $('#modal-stock-transactions .ui.form').submit();
          if (!$('#modal-stock-transactions .ui.form').form('is valid'))
            return false;

          Api.postStocksTransactions(StockTransactionModal.save(undefined), function() {
            draw();
          }, function(e) {
            Util.errorMsg('Konnte Transaktion nicht erstellen.', e);
          });

          draw();
        },
        onVisible: function() {
          StockTransactionModal.load(selectedTransaction);
        }
      }).modal('show');
    }
  };

  Common.getSecuritiesAccounts().then(() => {
    table = new Table(opts);
    table.init();
    dataTable = table.table;

    StockTransactionModal.init();
  });
}

export function refresh() {
  table.refresh();
}
