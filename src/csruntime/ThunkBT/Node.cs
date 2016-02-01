using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ThunkBT
{
    public class Sequence : IO<Result>
    {
        private readonly ThunkList<IO<Result>> subTrees;
        public Sequence(ThunkList<IO<Result>> subTrees)
        {
            this.subTrees = subTrees;
        }
        public override Result Drive(Context ctx)
        {
            throw new NotImplementedException();
        }
    }

    public class Select : IO<Result>
    {
        private readonly ThunkList<IO<Result>> subTrees;
        public Select(ThunkList<IO<Result>> subTrees)
        {
            this.subTrees = subTrees;
        }

        public override Result Drive(Context ctx)
        {
            throw new NotImplementedException();
        }
    }

    public class Box<T> : Thunk<T>
    {
        private T inner = default(T);

        public T SetVal(T v)
        {
            T ret = inner;
            inner = v;

            return ret;
        }

        public override T GetUserValue()
        {
            return inner;
        }
    }

    public class With<T, TR> : IO<TR>
    {
        private readonly Box<T> box;
        private readonly Thunk<IO<T>> ioGet;
        private readonly Thunk<IO<TR>> subTree;

        public With(Box<T> box, Thunk<IO<T>> ioGet, Thunk<IO<TR>> subTree)
        {
            this.box = box;
            this.ioGet = ioGet;
            this.subTree = subTree;
        }
        public override TR Drive(Context ctx)
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
                    value = (T)thisContinuation.Param;
                }
            }

            if (!skipIoGet)
            {
                value = ioGet.GetUserValue().Drive(ctx);

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
            var ret = subTree.GetUserValue().Drive(ctx);

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
