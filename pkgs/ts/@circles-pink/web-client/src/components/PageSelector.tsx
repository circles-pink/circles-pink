import React, { ReactElement, SetStateAction } from 'react';
import tw from 'twin.macro';
import { Page } from '../onboarding/utils/paginate';
import { Button } from './forms';

type PageSelectorProps = {
  currentPage: number;
  setCurrentPage: React.Dispatch<SetStateAction<number>>;
  pageControls: Page[];
};

export const PageSelector = ({
  currentPage,
  setCurrentPage,
  pageControls,
}: PageSelectorProps): ReactElement => (
  <>
    {pageControls.map(pagenumber => {
      const nextpage: number = getNextPage(currentPage, pagenumber);
      return (
        <PageSelectorRow key={pagenumber}>
          <Button
            key={pagenumber}
            onClick={() => setCurrentPage(nextpage)}
            prio={currentPage === pagenumber ? 'high' : 'low'}
          >
            {pagenumber}
          </Button>
        </PageSelectorRow>
      );
    })}
  </>
);

// -----------------------------------------------------------------------------
// UI
// -----------------------------------------------------------------------------

const PageSelectorRow = tw.div`mt-2`;

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
