import React, { ReactElement, ReactNode, useEffect, useState } from 'react';
import tw from 'twin.macro';
import useDimensions, { ElementDimensions } from 'use-element-dimensions';
import * as A from 'fp-ts/Array';
import * as R from 'fp-ts/Random';
import * as IO from 'fp-ts/IO';
import { pipe } from 'fp-ts/lib/function';
import { number } from 'fp-ts';
import { NonEmptyArray } from 'fp-ts/lib/NonEmptyArray';

// -----------------------------------------------------------------------------
// Types
// -----------------------------------------------------------------------------

type StepExtra = { position: number };

type Step = { label: string };

// -----------------------------------------------------------------------------
// Constants
// -----------------------------------------------------------------------------

const colors = {
  green: '#80ffbf',
  grey: '#cccccc',
};

// -----------------------------------------------------------------------------
// Hooks
// -----------------------------------------------------------------------------

const useAnimation = (): number => {
  const [time, setTime] = useState<number>(0);

  useEffect(() => {
    const tick = (n: number) => {
      setTime(n);
      requestAnimationFrame(tick);
    };
    tick(performance.now());
  }, []);

  return time;
};

// -----------------------------------------------------------------------------
// Hooks / useExtraData
// -----------------------------------------------------------------------------

type ExtraData = { steps: (Step & StepExtra)[] };

const useExtraData = (old: { steps: Step[] }) => {
  const [data, setData] = useState<null | ExtraData>(null);

  useEffect(() => {
    const newData = pipe(
      old.steps,
      A.traverse(IO.Applicative)(o =>
        pipe(
          R.random,
          IO.map(position => ({ ...o, position }))
        )
      )
    );

    setData({
      steps: newData(),
    });
  }, [JSON.stringify(old)]);

  return data;
};

// -----------------------------------------------------------------------------
// Hooks / usePrevSelected
// -----------------------------------------------------------------------------

type PrevSelected = {
  selected: number;
  prevSelected: number;
  lastAction: number;
};

const usePrevSelected = (newSelected: number): PrevSelected => {
  const [prevSelected, setPrevSelected] = useState<PrevSelected>({
    selected: 0,
    prevSelected: 0,
    lastAction: 0,
  });
  useEffect(() => {
    setPrevSelected(s => ({
      selected: newSelected,
      prevSelected: s.selected,
      lastAction: performance.now(),
    }));
  }, [newSelected]);

  return prevSelected;
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
// Components / StepIndicator
// -----------------------------------------------------------------------------

export type StepIndicatorProps = {
  steps?: NonEmptyArray<Step>;
  selected?: number;
  height?: number;
  debug?: boolean;
  circleRadius?: number;
  speed: number;
};

export const StepIndicator = ({
  selected = 0,
  steps = [{ label: 'A' }],
  height = 80,
  debug = false,
  circleRadius = 10,
  speed = 0.001,
}: StepIndicatorProps): ReactElement => {
  // Hooks
  const countDots = steps.length;
  const selected_ = selected % countDots;

  const extraData = useExtraData({ steps });
  const prevSelected = usePrevSelected(selected_);
  const [size, ref] = useDimensions();
  const time = useAnimation();

  if (!extraData) return <></>;

  const props = {
    ...extraData,
    ...prevSelected,
    selected: selected_,
    width: size.width,
    height: size.height,
    time,
    debug,
    circleRadius,
    speed: speed,
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
    PrevSelected,
    Required<Pick<StepIndicatorProps, 'debug' | 'circleRadius' | 'speed'>> {
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
  norm(Math.sin(pos * Math.PI + time * speed)) * height;

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
  } = props;

  const y = getPos(step.position, time, height, speed);

  const dir = prevSelected < selected ? 0 : -1;
  const color =
    index < selected ||
    (dir == 0 && index == selected && relTime > 900) ||
    (dir == -1 && index == selected)
      ? colors.green
      : colors.grey;
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

const Line = ({ x1, y1, x2, y2, m, circleRadius }: LineProps) => {
  const xm = x1 + (x2 - x1) * m;
  const ym = y1 + (y2 - y1) * m;

  return (
    <>
      <line
        x1={x1}
        y1={y1}
        x2={xm}
        y2={ym}
        stroke={colors.green}
        style={{ strokeWidth: circleRadius / 2 }}
      />
      <line
        x1={xm}
        y1={ym}
        x2={x2}
        y2={y2}
        stroke={colors.grey}
        style={{ strokeWidth: circleRadius / 2 }}
      />
    </>
  );
};
