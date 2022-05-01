import React from 'react';
import tw from 'twin.macro';
import { Claim } from './text';

type ListElementProps = {
  title?: string;
};

export const ListElement = (props: ListElementProps) => {
  return (
    <Frame>
      <Claim color="white">{props.title}</Claim>
      <TableContainer>
        <Table>
          <TableHeader>
            <tr className="bg-gray-50">
              <TableHead>User</TableHead>
              <TableHead>Safe</TableHead>
              <TableHead>Relation</TableHead>
              <TableHead>Transferable</TableHead>
            </tr>
          </TableHeader>

          <TableBody>
            <tr>
              <TableData>x</TableData>
              <TableData>0xe33...</TableData>
              <TableData>In / Out</TableData>
              <TableData>25€</TableData>
            </tr>
          </TableBody>
        </Table>
      </TableContainer>
    </Frame>
  );
};

// -----------------------------------------------------------------------------
// UI
// -----------------------------------------------------------------------------

const Frame = tw.div`block p-8 bg-gray-300 border border-gray-800 shadow-xl rounded-xl text-white m-2`;
const TableContainer = tw.div`overflow-hidden overflow-x-auto border border-gray-100 rounded`;
const Table = tw.table`min-w-full text-sm divide-y divide-gray-200`;
const TableHeader = tw.thead`px-4 py-2 font-medium text-left text-gray-900 whitespace-nowrap`;
const TableHead = tw.th`px-4 py-2 font-medium text-left text-gray-900 whitespace-nowrap`;
const TableBody = tw.tbody`divide-y divide-gray-100`;
const TableData = tw.td`px-4 py-2 text-gray-700 whitespace-nowrap`;
