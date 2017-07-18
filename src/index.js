import FastClick from 'fastclick';
import _ from 'lodash';
import Rx from 'rx';
import moment from 'moment';
import Cycle from '@cycle/core';
import { makeHTTPDriver } from '@cycle/http';
import {
  small,
  table,
  th,
  td,
  tr,
  span,
  button,
  ul,
  li,
  div,
  label,
  input,
  h1,
  h2,
  h3,
  makeDOMDriver
} from '@cycle/dom';

FastClick.attach(document.body);

const FIREBASE = 'https://olive-the-things.firebaseio.com';
const EVENTS_URL = `${FIREBASE}/events.json`;

function lastEvent(events) {
  return _.chain(events)
    .sortByOrder('time', 'desc')
    .first()
    .thru((event) => {
      if (event && event.time) {
        var t = moment(event.time);
        var totalMinutes = moment().diff(t, 'minutes') || 1;

        // don't need to be precise past 24 hours
        if (totalMinutes > 1440) {
          return `${t.fromNow()} at ${t.format('HH:mm')}`;
        }

        var minutes = totalMinutes % 60;
        var hours = Math.floor((totalMinutes) / 60);

        var minutesMessage = minutes ? `${minutes} min` : '';
        var hoursMessage = hours ? `${hours} hr` : '';

        return `${hoursMessage} ${minutesMessage} ago at ${t.format('HH:mm')}`;
      } else {
        return 'none';
      }
    })
    .value();
}

function nextFeeding(events) {
  return _.chain(events)
    .sortByOrder('time', 'desc')
    .first()
    .thru((event) => {
      if (event && event.time) {
        var t = moment(event.time);
        return `don't go past ${t.add(4, 'hours').format('HH:mm')}`;
      } else {
        return 'n/a';
      }
    })
    .value();
}

function lastMedication(medicationEvents, medicationName) {
  return _.chain(medicationEvents)
    .filter('name', medicationName)
    .thru(lastEvent)
    .value();
}


function main(sources) {

  const timeshift$ = sources.DOM.select('.timeshift').events('input')
    .map((ev) => { return parseInt(ev.target.value, 10) * -1 })
    .startWith(0);

  const eventRequest$ = Rx.Observable.merge(
      Rx.Observable.interval(1000).filter(() => { return moment().second() === 1 }), // only refresh on minute
      sources.DOM.select('.refresh').events('click'),
      sources.HTTP.filter(res$ => res$.request.method !== 'GET').mergeAll() // refresh for every post
    )
    .startWith(null)
    .map(() => {
      return {
        url: EVENTS_URL,
        method: 'GET',
        query: {
          orderBy: '"time"',
          startAt: '"2017-07-13T00:00:00.000Z"',
        }
      };
    });

  const poopCheck$ = sources.DOM.select('.poop').events('change')
    .map(ev => ev.target.checked)
    .startWith(false);
  const peeCheck$ = sources.DOM.select('.pee').events('change')
    .map(ev => ev.target.checked)
    .startWith(false);

  const postFeed$ = sources.DOM.select('.feed').events('click')
    .withLatestFrom(
      timeshift$,
      (nope, timeshift) => { return timeshift; }
    )
    .map((timeshift) => {
      return {
        url: EVENTS_URL,
        method: 'POST',
        send: {
          type: 'feeding',
          time: moment().add(timeshift, 'minutes').toISOString(),
          who: 'olive'
        }
      }
    });

  const postMedication$ = Rx.Observable.merge(
      sources.DOM.select('.ibuprofen').events('click').map(() => 'ibuprofen'),
      sources.DOM.select('.tylenol').events('click').map(() => 'tylenol'),
      sources.DOM.select('.iron').events('click').map(() => 'iron'),
      sources.DOM.select('.prenatal').events('click').map(() => 'prenatal'),
      sources.DOM.select('.vitamind').events('click').map(() => 'vitamind'),
      sources.DOM.select('.colace').events('click').map(() => 'colace')
    )
    .withLatestFrom(
      timeshift$,
      (name, timeshift) => { return {name, timeshift}; }
    )
    .map(({name, timeshift}) => {
      return {
        url: EVENTS_URL,
        method: 'POST',
        send: {
          time: moment().add(timeshift, 'minutes').toISOString(),
          type: 'medication',
          name: name,
          who: 'andrea'
        }
      };
    });

  const eventsByType$ = sources.HTTP
    .filter(res$ => res$.request.method === 'GET')
    .mergeAll()
    .map((res) => {
      return _.chain(res.body)
        .values()
        .sortByOrder('time', 'desc')
        .groupBy('type')
        .value()
    })
    .startWith(null);

  const vtree$ = Rx.Observable.combineLatest(
      eventsByType$,
      timeshift$,
      (eventsByType, timeshift) => { return { eventsByType, timeshift }; }
    )
    .map(({eventsByType, timeshift}) => {
      return (
        div([
          eventsByType ? [
            h3('.right', [
              span(`Refreshed at ${moment().format('HH:mm')}`),
              button('.refresh .pure-button', '↻')
            ]),
            // h2(lastEvent(eventsByType.feeding)),
            table('.highlights .pure-table .pure-table-horizontal', [
              tr([
                th('Feeding'),
                td([
                  div(lastEvent(eventsByType.feeding))
                ])
              ]),
              tr([
                th('Tylenol'),
                td(lastMedication(eventsByType.medication, 'tylenol'))
              ]),
              tr([
                th('Ibuprofen'),
                td(lastMedication(eventsByType.medication, 'ibuprofen'))
              ]),
              tr([
                th('Iron'),
                td(lastMedication(eventsByType.medication, 'iron'))
              ]),
              tr([
                th('Prenatal'),
                td(lastMedication(eventsByType.medication, 'prenatal'))
              ]),
              tr([
                th('Vitamin D'),
                td(lastMedication(eventsByType.medication, 'vitamind'))
              ]),
              tr([
                th('Colace'),
                td(lastMedication(eventsByType.medication, 'colace'))
              ]),
            ]),
          ] : div('Loading...'),
          div('.center', [
            span(`shift mins: `),
            input('.timeshift', {attributes: {type: 'number', inputmode: 'numeric', pattern: '[0-9]*'}}),
          ]),
          div('.buttons .right', [
            div([
              button('.feed .pure-button .pure-button-primary', 'Feed'),
            ]),
            div([
              button('.tylenol .pure-button .pure-button-primary', 'Tylenol'),
            ]),
            div([
              button('.ibuprofen .pure-button .pure-button-primary', 'Ibuprofen'),
            ]),
            div([
              button('.iron .pure-button .pure-button-primary', 'Iron'),
            ]),
            div([
              button('.prenatal .pure-button .pure-button-primary', 'Prenatal'),
            ]),
            div([
              button('.vitamind .pure-button .pure-button-primary', 'Vitamin D'),
            ]),
            div([
              button('.colace .pure-button .pure-button-primary', 'Colace'),
            ]),
            // div([
            //   label([
            //     input('.poop', {attributes: {type: 'checkbox'}}),
            //     'poop',
            //   ]),
            //   label([
            //     input('.pee', {attributes: {type: 'checkbox'}}),
            //     'pee',
            //   ]),
            // ]),
          ]),
        ])
      );
    });

  const sinks = {
    DOM: vtree$,
    HTTP: Rx.Observable.merge(
      eventRequest$,
      postFeed$,
      postMedication$
    )
  };

  return sinks;
}

Cycle.run(main, {
  DOM: makeDOMDriver('#app'),
  HTTP: makeHTTPDriver()
});
