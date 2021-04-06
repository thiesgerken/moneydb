'use strict';

import Table from '/table.js';
import $ from '/jquery.js';
import * as Util from '/util.js';
import * as Api from '/moneydb.js';
import * as Common from '/common.js';
import * as AccountModal from './accounts_modal.js';

export let table;
export let location;
export let dataTable;

export function init(loc) {
  location = $(loc);

  let opts = {
    url: 'accounts/rendered',
    location: location,
    defaultOrder: [
      [7, 'asc'],
      [6, 'asc'],
      [0, 'asc']
    ],
    lengths: [25, 50],
    expandable: false,
    formatter: function() {
      // s = '<div class="ui relaxed divided list">' +
      //   '<div class="item"><div class="content"><div class="header">Typ</div>' +
      //   '<div class="description">' + data.typeDetailed + '</div></div></div>';
      //
      // s += '<div class="item"><div class="content"><div class="header">Anzahl an verbundenen Ausgaben</div>' +
      //   '<div class="description">' + data.expenseCount + '</div></div></div>';
      //
      // s += '<div class="item"><div class="content"><div class="header">Anzahl an verbundenen Kontost&auml;nden</div>' +
      //   '<div class="description">' + data.balanceCount + '</div></div></div>';
      //
      // if (data.description != "")
      //   s += '<div class="item"><div class="content"><div class="header">Beschreibung</div>' +
      //   '<div class="description">' + data.description + '</div></div></div>';
      //
      // return s + '</div>';
      return '';
    },
    isReadOnly: () => false,
    isDeletable: () => true,
    colorRow: function(row, data) {
      Util.setBackgroundColor($(row).find('td:nth-child(3)'), data.color, false);
    },
    colorRowDark: function(row, data) {
      Util.setBackgroundColor($(row).find('td:nth-child(3)'), data.color, true);
    },
    createdRow: function() {},
    columns: [{
      data: 'id',
      title: 'ID',
      width: '2.5em'
    }, {
      data: 'title',
      title: 'Titel',
      width: '16em'
    }, {
      data: 'color',
      title: 'Farbe',
      width: '5em'
    }, {
      data: 'description',
      title: 'Beschreibung'
    }, {
      data: 'syncing',
      title: 'Synchronisation',
      width: '20em',
      orderable: false,
      render: function(data, type, row) {
        if (row.syncing) {
          let s = '';

          if (row.syncing.sign)
            s += 'gemeinsam mit ';
          else
            s += 'gegenteilig zu ';

          let aid = row.id == row.syncing.account1 ? row.syncing.account2 : row.syncing.account1;
          s += Common.accountToString(Common.findAccount(aid));
          return s;
        } else
          return '';
      }
    }, {
      data: 'availability',
      title: 'Verfügbarkeit',
      width: '8em',
      render: function(data, type, row) {
        switch (row.availability) {
          case 'Immediately':
            return 'Sofort';
          case 'Weeks':
            return 'Wochen';
          case 'Months':
            return 'Monate';
          case 'Years':
            return 'Jahre';
          case 'Decades':
            return 'Jahrzehnte';
          default:
            return row.availability;
        }
      }
    }, {
      data: 'kind',
      title: 'Art',
      width: '6em',
      render: function(data, type, row) {
        switch (row.kind) {
          case 'Cash':
            return 'Bargeld';
          case 'Credit':
            return 'Kredit';
          case 'Debit':
            return 'Giro';
          case 'Debt':
            return 'Schulden';
          case 'Virtual':
            return 'Virtuell';
          case 'Investment':
            return 'Investiert';
          case 'Prepayment':
            return 'Vorauszahlung';
          default:
            return row.kind;
        }
      }
    }, {
      data: 'hidden',
      title: 'Versteckt',
      width: '5em',
      render: function(data, type, row) {
        if (row.hidden)
          return 'Ja';
        else return 'Nein';
      }
    }],
    removeHandler: function(item, draw) {
      $('#modal-delete p').html('Konto <b>' + item.id + '</b> (<b>' + item.title + '</b>) wirklich löschen? Dies kann nicht rückgängig gemacht werden!');
      $('#modal-delete').modal({
        closable: false,
        onApprove: function() {
          Api.deleteAccountsRenderedById(item.id, function() {
            draw();
          }, function(e) {
            Util.errorMsg('Konnte Konto nicht löschen.', e);
          });
        }
      }).modal('show');
    },
    addHandler: function(draw) {
      $('#modal-accounts .header').html('Konto erstellen');
      AccountModal.reset();

      $('#modal-accounts').modal({
        observeChanges: true,
        allowMultiple: true,
        closable: false,
        onApprove: function() {
          $('#modal-accounts .ui.form').submit();
          if (!$('#modal-accounts .ui.form').form('is valid'))
            return false;

          Api.postAccountsRendered(AccountModal.save(undefined), function() {
            draw();
          }, function(e) {
            Util.errorMsg('Konnte Konto nicht erstellen.', e);
          });

          draw();
        },
        onVisible: function() {
          AccountModal.reset();
        }
      }).modal('show');
    },
    modifyHandler: function(selectedAccount, draw) {
      $('#modal-accounts .header').html('Konto bearbeiten');
      AccountModal.load(selectedAccount);

      $('#modal-accounts').modal({
        observeChanges: true,
        allowMultiple: true,
        closable: false,
        onApprove: function() {
          $('#modal-accounts .ui.form').submit();
          if (!$('#modal-accounts .ui.form').form('is valid'))
            return false;

          Api.putAccountsRenderedById(selectedAccount.id, AccountModal.save(selectedAccount), function() {
            draw();
          }, function(e) {
            Util.errorMsg('Konnte Konto nicht bearbeiten.', e);
          });

          draw();
        },
        onVisible: function() {
          AccountModal.load(selectedAccount);
        }
      }).modal('show');
    },
    duplicateHandler: function(selectedAccount, draw) {
      $('#modal-accounts .header').html('Konto kopieren');
      AccountModal.load(selectedAccount);

      $('#modal-accounts').modal({
        observeChanges: true,
        allowMultiple: true,
        closable: false,
        onApprove: function() {
          $('#modal-accounts .ui.form').submit();
          if (!$('#modal-accounts .ui.form').form('is valid'))
            return false;

          Api.postAccountsRendered(AccountModal.save(undefined), function() {
            draw();
          }, function(e) {
            Util.errorMsg('Konnte Konto nicht erstellen.', e);
          });

          draw();
        },
        onVisible: function() {
          AccountModal.load(selectedAccount);
        }
      }).modal('show');
    }
  };

  table = new Table(opts);
  table.init();
  dataTable = table.table;

  AccountModal.init();
}

export function refresh() {
  table.refresh();
}
