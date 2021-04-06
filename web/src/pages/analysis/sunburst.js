'use strict';

import moment from 'moment/src/moment.js';
import $ from 'jquery';
import _ from 'lodash';
import * as Util from '/util.js';
import * as Common from '/common.js';
import * as d3 from 'd3';

// Given a node in a partition layout, return an array of all of its ancestor
// nodes, highest first, but excluding the root.
let getAncestors = function(node) {
  let path = [];
  let current = node;
  while (current.parent) {
    path.unshift(current);
    current = current.parent;
  }
  return path;
};

export function create(location, data, width, height) {
  let radius = Math.min(width, height) / 2;

  $(location).empty();
  let svg = d3.select($(location).get(0)).append('svg')
    .attr('width', width)
    .attr('height', height)
    .attr('style', 'display: block; margin: auto')
    .append('g')
    .attr('transform', 'translate(' + width / 2 + ',' + height * .5 + ')');

  let mainText = svg.append('text').attr('x', 0)
    .attr('y', 0)
    .attr('font-family', 'sans-serif')
    .attr('font-size', '1em')
    .attr('text-anchor', 'middle');

  let detailText = svg.append('text').attr('x', 0)
    .attr('y', '1.1em')
    .attr('font-family', 'sans-serif')
    .attr('font-size', '0.9em')
    .attr('text-anchor', 'middle');

  mainText.text(data.title);
  detailText.text(data.detail);

  let mouseOver = function(svg) {
    return function(d) {
      let sequenceArray = getAncestors(d);
      svg.selectAll('path')
        .style('opacity', 0.6);

      svg.selectAll('path')
        .filter(function(node) {
          return (sequenceArray.indexOf(node) >= 0);
        })
        .style('opacity', 1);

      mainText.text(Util.textEllipsis(d.data.title, 24));
      detailText.text(d.data.detail);
    };
  };

  d3.select($(location).get(0)).on('mouseleave', function() {
    // svg.selectAll("path").on("mouseover", null);
    svg.selectAll('path')
      .transition()
      .duration(750)
      .style('opacity', 1);

    mainText.text(data.title);
    detailText.text(data.detail);
  });

  let rootNode = d3.hierarchy(data)
    .sum(function(d) {
      if (d.children.length == 0) return d.value;
      else return 0;
    });

  let root = d3.partition()
    .size([2 * Math.PI, radius * radius])(rootNode);

  let arc = d3.arc()
    .startAngle(d => d.x0)
    .endAngle(d => d.x1)
    .padAngle(d => Math.min((d.x1 - d.x0) / 2, 0.005))
    .innerRadius(d => Math.sqrt(d.y0))
    .outerRadius(d => Math.sqrt(d.y1));

  svg.selectAll('path')
    .data(root.descendants().slice(1))
    .enter().append('path')
    .attr('display', function(d) {
      return d.depth ? null : 'none';
    }) // hide inner ring
    .attr('d', arc)
    .style('stroke', '#fff')
    .style('fill', function(d) {
      if (d.data.color)
        return d.data.color;
      else return '#fff';
      // return tinycolor(d.statColor).saturate(20).toString();
    })
    .on('mouseover', mouseOver(svg));

  d3.select(self.frameElement).style('height', height + 'px');
}

export function createBig(location, data) {
  let width = $(location).width() * 0.65;
  create(location, data, width, width * 0.85);
}

export function createSmall(location, data) {
  let width = $(location).width() * 0.65;
  create(location, data, width, width * 0.85);
}

export function categoryData(d, title) {
  let x = {};

  if (typeof title != 'undefined')
    x.title = title;
  else
    x.title = d.title;

  x.value = d.amount;
  x.color = d.color;
  x.detail = Util.formatAmount(d.amount, '€') + (d.date != null ? (' am ' + moment(d.date).format('L')) : '');

  x.children = _.map(d.children, d => categoryData(d));
  x.children = _.sortBy(x.children, o => -1 * o.value);

  return x;
}

export function balanceData(d, title, sign = 1) {
  let x = {};

  if (typeof title != 'undefined')
    x.title = title;
  else
    x.title = null;

  let colors = ['#3ac400', '#b0ea1e', '#d3d013', '#ea8519', '#ea5819'];

  x.value = 0;
  x.color = null;
  x.detail = null;
  x.children = _.map(['Sofort', 'Wochen', 'Monate', 'Jahre', 'Jahrzehnte'], (y, i) => ({
    title: y,
    color: colors[i], // tinycolor('grey').lighten(i * 5).toString(),
    children: [],
    value: 0,
    detail: null
  }));

  _.forEach(d.balances, function(b) {
    let a = Common.findAccount(b.accountId);

    let idx = _.findIndex(['Immediately', 'Weeks', 'Months', 'Years', 'Decades'], y => y == a.availability);

    if (sign * b.amount > 0) {
      x.children[idx].value += Math.abs(b.amount);
      x.children[idx].children.push({
        title: a.title,
        color: a.color,
        value: Math.abs(b.amount),
        children: [],
        detail: Util.formatAmount(b.amount, '€')
      });
    }
  });

  x.children = _.map(x.children, function(b) {
    x.value += b.value;
    b.detail = Util.formatAmount(b.value, '€');
    return b;
  });

  x.detail = Util.formatAmount(x.value, '€');
  return x;
}
