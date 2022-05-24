import React, { ReactElement, SetStateAction } from 'react';
import tw from 'twin.macro';
import { Theme } from '../context/theme';
import { Page } from '../onboarding/utils/paginate';
import { Button } from './forms';
import { JustifyAround } from './helper';

type PageSelectorProps = {
  currentPage: number;
  setCurrentPage: React.Dispatch<SetStateAction<number>>;
  pageControls: Page[];
  theme: Theme;
};

export const PageSelector = ({
  currentPage,
  setCurrentPage,
  pageControls,
  theme,
}: PageSelectorProps): ReactElement => (
  <JustifyAround>
    {pageControls.map(pagenumber => {
      const nextpage: number = getNextPage(currentPage, pagenumber);
      return (
        <PageSelectorRow key={pagenumber}>
          <Button
            theme={theme}
            key={pagenumber}
            onClick={() => setCurrentPage(nextpage)}
            prio={currentPage === pagenumber ? 'high' : 'low'}
          >
            {pagenumber}
          </Button>
        </PageSelectorRow>
      );
    })}
  </JustifyAround>
);

// -----------------------------------------------------------------------------
// UI
// -----------------------------------------------------------------------------

const PageSelectorRow = tw.div`mx-2 mt-2`;

// -----------------------------------------------------------------------------
// Util
// -----------------------------------------------------------------------------

const getNextPage = (currentPage: number, pagenumber: Page): number => {
  switch (pagenumber) {
    case '>>':
      return currentPage + 1;
    case '<<':
      return currentPage - 1;
    default:
      return pagenumber;
  }
};
