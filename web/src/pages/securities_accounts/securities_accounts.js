'use strict';

import Table from '/table.js';
import $ from '/jquery.js';
import * as Util from '/util.js';
import * as Api from '/moneydb.js';
import * as SecuritiesAccountModal from './securities_accounts_modal.js';

export let table;
export let location;
export let dataTable;

export function init(loc) {
  location = $(loc);

  let opts = {
    url: 'stocks/accounts',
    location: location,
    defaultOrder: [
      [1, 'asc'],
      [2, 'asc'],
      [0, 'asc']
    ],
    lengths: [25, 50],
    expandable: false,
    formatter: () => '',
    isReadOnly: () => false,
    isDeletable: () => true,
    colorRow: () => {},
    colorRowDark: () => {},
    createdRow: () => {},
    columns: [{
      data: 'id',
      title: 'ID',
      width: '2.5em'
    }, {
      data: 'broker',
      title: 'Broker',
      width: '8em'
    }, {
      data: 'iban',
      title: 'IBAN',
      width: '15em'
    }, {
      data: 'title',
      title: 'Titel'
    }],
    removeHandler: function(item, draw) {
      $('#modal-delete p').html('Depot <b>' + item.id + '</b> (<b>' + item.title + '</b>) wirklich löschen? Dies kann nicht rückgängig gemacht werden!');
      $('#modal-delete').modal({
        closable: false,
        onApprove: function() {
          Api.deleteStocksAccountsById(item.id, function() {
            draw();
          }, function(e) {
            Util.errorMsg('Konnte Depot nicht löschen.', e);
          });
        }
      }).modal('show');
    },
    addHandler: function(draw) {
      $('#modal-securities-accounts .header').html('Depot erstellen');
      SecuritiesAccountModal.reset();

      $('#modal-securities-accounts').modal({
        observeChanges: true,
        allowMultiple: true,
        closable: false,
        onApprove: function() {
          $('#modal-securities-accounts .ui.form').submit();
          if (!$('#modal-securities-accounts .ui.form').form('is valid'))
            return false;

          Api.postStocksAccounts(SecuritiesAccountModal.save(undefined), function() {
            draw();
          }, function(e) {
            Util.errorMsg('Konnte Depot nicht erstellen.', e);
          });

          draw();
        },
        onVisible: function() {
          SecuritiesAccountModal.reset();
        }
      }).modal('show');
    },
    modifyHandler: function(selectedAccount, draw) {
      $('#modal-securities-accounts .header').html('Depot bearbeiten');
      SecuritiesAccountModal.load(selectedAccount);

      $('#modal-securities-accounts').modal({
        observeChanges: true,
        allowMultiple: true,
        closable: false,
        onApprove: function() {
          $('#modal-securities-accounts .ui.form').submit();
          if (!$('#modal-securities-accounts .ui.form').form('is valid'))
            return false;

          Api.putStocksAccountsById(selectedAccount.id, SecuritiesAccountModal.save(selectedAccount), function() {
            draw();
          }, function(e) {
            Util.errorMsg('Konnte Depot nicht bearbeiten.', e);
          });

          draw();
        },
        onVisible: function() {
          SecuritiesAccountModal.load(selectedAccount);
        }
      }).modal('show');
    },
    duplicateHandler: function(selectedAccount, draw) {
      $('#modal-securities-accounts .header').html('Depot kopieren');
      SecuritiesAccountModal.load(selectedAccount);

      $('#modal-securities-accounts').modal({
        observeChanges: true,
        allowMultiple: true,
        closable: false,
        onApprove: function() {
          $('#modal-securities-accounts .ui.form').submit();
          if (!$('#modal-securities-accounts .ui.form').form('is valid'))
            return false;

          Api.postStocksAccounts(SecuritiesAccountModal.save(undefined), function() {
            draw();
          }, function(e) {
            Util.errorMsg('Konnte Depot nicht erstellen.', e);
          });

          draw();
        },
        onVisible: function() {
          SecuritiesAccountModal.load(selectedAccount);
        }
      }).modal('show');
    }
  };

  table = new Table(opts);
  table.init();
  dataTable = table.table;

  SecuritiesAccountModal.init();
}

export function refresh() {
  table.refresh();
}
