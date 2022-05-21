import React from 'react';
import { Button } from './forms';

export const PageSelector = ({ currentPage, setCurrentPage, pageControls }) => {
  return pageControls.map(pagenumber => {
    let nextpage = pagenumber;
    if (pagenumber === '>>') {
      nextpage = currentPage + 1;
    } else if (pagenumber === '<<') {
      nextpage = currentPage - 1;
    }
    return (
      <Button
        key={pagenumber}
        onClick={() => setCurrentPage(nextpage)}
        prio={currentPage === pagenumber ? 'high' : 'low'}
      >
        {pagenumber}
      </Button>
    );
  });
};
