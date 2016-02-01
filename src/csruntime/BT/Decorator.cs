using System;
using System.CodeDom;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace BT
{
    public class Check : IO<Result>
    {
        private readonly IO<bool> subTree;

        public Result Drive(Context ctx)
        {
            throw new NotImplementedException();
        }
    }

    public class Box<T>
    {
        private T inner = default(T);

        public T SetVal(T v)
        {
            T ret = inner;
            inner = v;

            return ret;
        }

        public T GetVal()
        {
            return inner;
        }
    }

    public class With<T, TR> : IO<TR>
    {
        private readonly Box<T> box;
        private readonly IO<T> ioGet;
        private readonly IO<TR> subTree;

        public With(Box<T> box, IO<T> ioGet, IO<TR> subTree)
        {
            this.box = box;
            this.ioGet = ioGet;
            this.subTree = subTree;
        }
        public TR Drive(Context ctx)
        {
            var thisContinuation = ctx.Continuation;
            var value = default(T);
            var skipIoGet = false;

            if (thisContinuation != null)
            {
                // Continuation
                ctx.Continuation = thisContinuation.SubContinuation;

                // 0表示需要继续ioGet
                // 1表示需要继续subTree
                if (thisContinuation.NextStep == 1)
                {
                    skipIoGet = true;
                    value = (T) thisContinuation.Param;
                }
            }

            if (!skipIoGet)
            {
                value = ioGet.Drive(ctx);

                if (ctx.Continuation != null)
                {
                    // ioGet抛出了Continue
                    if (thisContinuation == null)
                    {
                        thisContinuation = new Continuation()
                        {
                            SubContinuation = ctx.Continuation,
                            NextStep = 0,
                        };
                    }
                    else
                    {
                        thisContinuation.SubContinuation = ctx.Continuation;
                        thisContinuation.NextStep = 0;
                    }

                    ctx.Continuation = thisContinuation;

                    return default(TR);
                }
            }
            
            var oldValue = box.SetVal(value);
            var ret = subTree.Drive(ctx);

            box.SetVal(oldValue);

            if (ctx.Continuation != null)
            {
                // subTree抛出了Continue
                if (thisContinuation == null)
                {
                    thisContinuation = new Continuation()
                    {
                        SubContinuation = ctx.Continuation,
                    };
                }

                ctx.Continuation = thisContinuation;
                thisContinuation.Param = value;
            }

            return ret;
        }
    }
}
