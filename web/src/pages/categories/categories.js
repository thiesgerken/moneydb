'use strict';

import Table from '/table.js';
import $ from '/jquery.js';
import * as Util from '/util.js';
import * as Api from '/moneydb.js';
import * as Common from '/common.js';
import * as CategoryModal from './categories_modal.js';

export let table;
export let location;
export let dataTable;

export function init(loc) {
  location = $(loc);

  let opts = {
    url: 'categories/rendered',
    location: location,
    defaultOrder: [
      [0, 'desc']
    ],
    lengths: [25, 50],
    expandable: function(x) {
      return x.replaces.length > 0;
    },
    formatter: function(data) {
      let s = '<div class="ui relaxed divided list">';

      s += '<div class="item"><div class="content"><div class="header">Ersatz für Kategorien anderer Nutzer</div>' +
        '<div class="description"><ul class="ui list">';

      for (let i = 0; i < data.replaces.length; i++)
        s += '<li>' + Common.categoryToString(Common.findCategory(data.replaces[i])) + '</li>';

      s += '</ul></div></div></div>';

      return s + '</div>';
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
    }],
    removeHandler: function(item, draw) {
      $('#modal-delete p').html('Kategorie <b>' + item.id + '</b> (<b>' + item.title + '</b>) wirklich löschen? Dies kann nicht rückgängig gemacht werden!');

      $('#modal-delete').modal({
        closable: false,
        onApprove: function() {
          Api.deleteCategoriesRenderedById(item.id, function() {
            draw();
          }, function(e) {
            Util.errorMsg('Konnte Kategorie nicht löschen.', e);
          });
        }
      }).modal('show');
    },
    addHandler: function(draw) {
      $('#modal-categories .header').html('Kategorie erstellen');
      CategoryModal.reset();

      $('#modal-categories').modal({
        observeChanges: true,
        allowMultiple: true,
        closable: false,
        onApprove: function() {
          $('#modal-categories .ui.form').submit();
          if (!$('#modal-categories .ui.form').form('is valid'))
            return false;

          Api.postCategoriesRendered(CategoryModal.save(undefined), function() {
            draw();
          }, function(e) {
            Util.errorMsg('Konnte Kategorie nicht erstellen.', e);
          });

          draw();
        },
        onVisible: function() {
          CategoryModal.reset();
        }
      }).modal('show');
    },
    modifyHandler: function(selectedCategory, draw) {
      $('#modal-categories .header').html('Kategorie bearbeiten');
      CategoryModal.load(selectedCategory);

      $('#modal-categories').modal({
        observeChanges: true,
        allowMultiple: true,
        closable: false,
        onApprove: function() {
          $('#modal-categories .ui.form').submit();
          if (!$('#modal-categories .ui.form').form('is valid'))
            return false;

          Api.putCategoriesRenderedById(selectedCategory.id, CategoryModal.save(selectedCategory), function() {
            draw();
          }, function(e) {
            Util.errorMsg('Konnte Kategorie nicht bearbeiten.', e);
          });

          draw();
        },
        onVisible: function() {
          CategoryModal.load(selectedCategory);
        }
      }).modal('show');
    },
    duplicateHandler: function(selectedCategory, draw) {
      $('#modal-categories .header').html('Kategorie kopieren');
      CategoryModal.load(selectedCategory);

      $('#modal-categories').modal({
        observeChanges: true,
        allowMultiple: true,
        closable: false,
        onApprove: function() {
          $('#modal-categories .ui.form').submit();
          if (!$('#modal-categories .ui.form').form('is valid'))
            return false;

          Api.postCategoriesRendered(CategoryModal.save(undefined), function() {
            draw();
          }, function(e) {
            Util.errorMsg('Konnte Kategorie nicht erstellen.', e);
          });

          draw();
        },
        onVisible: function() {
          CategoryModal.load(selectedCategory);
        }
      }).modal('show');
    }
  };

  table = new Table(opts);
  table.init();
  dataTable = table.table;

  CategoryModal.init();
}

export function refresh() {
  table.refresh();
}
