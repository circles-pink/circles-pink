import React from 'react';
import '../src/css/index.css';

const Layout = ({ children }) => {
    return (
        <div className="px-20 py-10">
            {children}
        </div>
    )
}

export default Layout;