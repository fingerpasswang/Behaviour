using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace BT
{
    public enum Result
    {
        Continue,
        Success,
        Failure,
    }

    public class Continuation
    {
        public Continuation SubContinuation { get; set; }
        public int NextStep { get; set; }
        public object Param { get; set; }
    }

    public class Context
    {
        public Continuation Continuation { get; set; }
        public object Self { get; set; }
    }

    public interface IO<T>
    {
        T Drive(Context ctx);
    }
}
