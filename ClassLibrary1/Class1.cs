using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using Microsoft.Office.Interop.Excel;

namespace ClassLibrary1
{
    public class Class1
    {
        public static Microsoft.Office.Interop.Excel.Application GetApp()
        {
            return new Microsoft.Office.Interop.Excel.Application();
        }
    }

}
