import React, { ReactElement, ReactNode, useEffect, useState } from 'react';
import tw from 'twin.macro';
import useDimensions, { ElementDimensions } from 'use-element-dimensions';
import * as A from 'fp-ts/Array';
import * as R from 'fp-ts/Random';
import * as IO from 'fp-ts/IO';
import { pipe } from 'fp-ts/lib/function';
import { number } from 'fp-ts';
import { NonEmptyArray } from 'fp-ts/lib/NonEmptyArray';
import * as G from '@no-day/fp-ts-generators';

// -----------------------------------------------------------------------------
// Types
// -----------------------------------------------------------------------------

type StepExtra = { position: number };

type Step = { label: string };

type StepIndicatorTheme = {
  active: string;
  inActive: string;
};

// -----------------------------------------------------------------------------
// Constants
// -----------------------------------------------------------------------------

const colors = {
  active: 'hotpink',
  inActive: '#ebebeb',
};

// -----------------------------------------------------------------------------
// Hooks
// -----------------------------------------------------------------------------

const useAnimation = (): number => {
  const [time, setTime] = useState<number>(0);

  useEffect(() => {
    const tick = (n: number) => {
      setTime(performance.timeOrigin + n);
      requestAnimationFrame(tick);
    };
    tick(performance.now());
  }, []);

  return time;
};

// -----------------------------------------------------------------------------
// Utils
// -----------------------------------------------------------------------------

const easeInOutSine = (x: number): number => -(Math.cos(Math.PI * x) - 1) / 2;

const clamp =
  (l: number, h: number) =>
  (n: number): number =>
    n < l ? l : n > h ? h : n;

const norm = (n: number): number => (n + 1) / 2;

// -----------------------------------------------------------------------------
// Utils / addExtraData
// -----------------------------------------------------------------------------

type ExtraData = { steps: (Step & StepExtra)[] };

const gen = (n: number): G.Gen<number[]> =>
  G.vectorOf(n)(G.float({ min: 0, max: 1 }));

const seed = G.mkSeed(30985848);

const addExtraData = (steps: Step[]): (Step & StepExtra)[] => {
  const randomFloats = G.generate({ seed })(gen(steps.length));

  return A.zipWith(steps, randomFloats, (step, position) => ({
    ...step,
    position,
  }));
};

// -----------------------------------------------------------------------------
// Components / StepIndicator
// -----------------------------------------------------------------------------

export type StepIndicatorProps = {
  steps?: NonEmptyArray<Step>;
  height?: number;
  debug?: boolean;
  circleRadius?: number;
  speed?: number;
  selected?: number;
  prevSelected?: number;
  lastAction?: number;
  theme?: StepIndicatorTheme;
};

export const StepIndicator = ({
  steps = [{ label: 'A' }],
  height = 80,
  debug = false,
  circleRadius = 10,
  speed = 0.001,
  selected = 0,
  prevSelected = 0,
  lastAction = 0,
  theme = {
    active: colors.active,
    inActive: colors.inActive,
  },
}: StepIndicatorProps): ReactElement => {
  // Hooks
  const countDots = steps.length;

  const [size, ref] = useDimensions();
  const time = useAnimation();

  const props = {
    steps: addExtraData(steps),
    selected: selected % countDots,
    prevSelected: prevSelected % countDots,
    lastAction,
    width: size.width,
    height: size.height,
    time,
    debug,
    circleRadius,
    speed: speed,
    theme,
  };

  return (
    <div ref={ref} style={{ height }}>
      <svg
        style={{
          ...(debug ? { outline: '1px solid red' } : {}),
          width: '100%',
          height: '100%',
        }}
        overflow="visible"
      >
        <StepIndicator_ {...props} />
      </svg>
    </div>
  );
};

// -----------------------------------------------------------------------------
// Components / StepIndicator_
// -----------------------------------------------------------------------------

interface StepIndicator_Props
  extends ExtraData,
    Required<
      Pick<
        StepIndicatorProps,
        | 'selected'
        | 'prevSelected'
        | 'lastAction'
        | 'debug'
        | 'circleRadius'
        | 'speed'
        | 'theme'
      >
    > {
  width: number;
  height: number;
  time: number;
}

const StepIndicator_ = (props: StepIndicator_Props) => {
  const { selected, width, steps, time, lastAction } = props;

  const countDots = steps.length;
  const countPanels = countDots - 1;
  const stepWidth = width / countPanels;
  const relTime = time - lastAction;

  const props_ = {
    ...props,
    selected,
    countDots,
    countPanels,
    stepWidth,
    relTime,
  };

  return (
    <>
      {steps.map((step, i) => (
        <svg x={i * stepWidth} overflow="visible">
          <Step {...props_} step={step} index={i} />
        </svg>
      ))}
    </>
  );
};

// -----------------------------------------------------------------------------
// Components / Step
// -----------------------------------------------------------------------------

interface StepProps extends StepIndicator_Props {
  step: Step & StepExtra;
  index: number;
  countDots: number;
  countPanels: number;
  stepWidth: number;
  relTime: number;
}

const getPos = (pos: number, time: number, height: number, speed: number) =>
  norm(Math.sin(pos * Math.PI + time * (speed * (0.75 + 0.25 * pos)))) * height;

export const Step = (props: StepProps): ReactElement => {
  const {
    step,
    selected,
    index,
    height,
    circleRadius,
    countPanels,
    time,
    relTime,
    prevSelected,
    speed,
    theme,
  } = props;

  const y = getPos(step.position, time, height, speed);

  const dir = prevSelected < selected ? 0 : -1;
  const color =
    index < selected ||
    (dir == 0 && index == selected && relTime > 900) ||
    (dir == -1 && index == selected)
      ? theme.active
      : theme.inActive;
  return (
    <>
      {index < countPanels ? <Panel {...props} /> : null}
      <circle
        r={circleRadius}
        cy={y}
        style={{
          fill: color,
        }}
      />
    </>
  );
};

// -----------------------------------------------------------------------------
// Components / Panel
// -----------------------------------------------------------------------------

interface PanelProps extends StepProps {}

const Panel = (props: PanelProps) => {
  const {
    stepWidth,
    steps,
    index,
    height,
    debug,
    selected,
    prevSelected,
    relTime,
    time,
    speed,
  } = props;
  const dir = prevSelected < selected ? 0 : -1;
  const animFwd = index == prevSelected && index + 1 == selected;
  const animBwd = index == selected && index + 1 == prevSelected;

  const m =
    animFwd || animBwd
      ? easeInOutSine(Math.abs(dir - clamp(0, 1)(relTime * 0.001)))
      : index < selected
      ? 1
      : 0;

  return (
    <>
      <Line
        {...props}
        x1={0}
        y1={getPos(steps[index].position, time, height, speed)}
        x2={stepWidth}
        y2={getPos(steps[index + 1].position, time, height, speed)}
        m={m}
      />
      {debug ? (
        <rect
          height={height}
          width={stepWidth}
          style={{
            fill: 'none',
            stroke: 'black',
            strokeWidth: 1,
          }}
          shapeRendering="crispEdges"
        />
      ) : null}
    </>
  );
};

// -----------------------------------------------------------------------------
// Components / Line
// -----------------------------------------------------------------------------

interface LineProps extends PanelProps {
  x1: number;
  y1: number;
  x2: number;
  y2: number;
  m: number;
}

const Line = ({ x1, y1, x2, y2, m, circleRadius, theme }: LineProps) => {
  const xm = x1 + (x2 - x1) * m;
  const ym = y1 + (y2 - y1) * m;

  return (
    <>
      <line
        x1={x1}
        y1={y1}
        x2={xm}
        y2={ym}
        stroke={theme.active}
        style={{ strokeWidth: circleRadius / 2.5 }}
      />
      <line
        x1={xm}
        y1={ym}
        x2={x2}
        y2={y2}
        stroke={theme.inActive}
        style={{ strokeWidth: circleRadius / 2.5 }}
      />
    </>
  );
};
