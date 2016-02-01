using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using HFSM;

namespace ContextFreeHFSM
{
    public class Continuation
    {
        public Continuation SubContinuation { get; set; }
        public int NextStep { get; set; }
        public object Param { get; set; }
    }

    public class Context<T>
    {
        public Continuation Continuation { get; set; }
        public T Self { get; set; }
    }

    public interface IState<TCleverUnit, TResult>
    {
        TResult Drive(Context<TCleverUnit> ctx);
    }

    public class PatrolState : IState<ICleverUnit, Result>
    {
        private readonly List<IState<ICleverUnit, Result>> subStates;
        public PatrolState()
        {
            subStates = new List<IState<ICleverUnit, Result>>()
            {
                new MoveToState(),
                new IdleState(),
            };
        }
        public Result Drive(Context<ICleverUnit> ctx)
        {
            var unit = ctx.Self.GetNearestTarget();
            if (unit != null)
            {
                ctx.Self.LockTarget(unit);

                return Result.Success;
            }

            var nextStep = 0;
            if (ctx.Continuation != null)
            {
                // Continuation
                var thisContinuation = ctx.Continuation;

                ctx.Continuation = thisContinuation.SubContinuation;

                var ret = subStates[nextStep].Drive(ctx);

                if (ret == Result.Continue)
                {
                    thisContinuation.SubContinuation = ctx.Continuation;
                    ctx.Continuation = thisContinuation;

                    return Result.Continue;
                }
                else if (ret == Result.Failure)
                {
                    ctx.Continuation = null;

                    return Result.Failure;
                }

                ctx.Continuation = null;
		        nextStep = thisContinuation.NextStep + 1;
            }

            for (; nextStep < subStates.Count; nextStep++)
            {
                var ret = subStates[nextStep].Drive(ctx);
				if (ret == Result.Continue)
				{
				    ctx.Continuation = new Continuation()
				    {
				        SubContinuation = ctx.Continuation,
				        NextStep = nextStep,
				    };

					return Result.Continue;
				} 
                else if (ret == Result.Failure) 
                {
                    ctx.Continuation = null;

					return Result.Failure;
				}
            }

            ctx.Continuation = null;

            return Result.Success;
        }
    }

    public class MoveToState : IState<ICleverUnit, Result>
    {
        public Result Drive(Context<ICleverUnit> ctx)
        {
            if (ctx.Continuation == null)
            {
                ctx.Self.MoveToNextPatrolPoiont();
            }

            if (ctx.Self.ReachCurrentPatrolPoint())
            {
                return Result.Success;
            }

            if (ctx.Continuation == null)
            {
                ctx.Continuation = new Continuation();
            }

            return Result.Continue;
        }
    }

    public class IdleState : IState<ICleverUnit, Result>
    {
        public Result Drive(Context<ICleverUnit> ctx)
        {
            if (ctx.Continuation == null)
            {
                ctx.Self.Idle();
                ctx.Continuation = new Continuation {Param = DateTime.Now};
            }

            if ((DateTime.Now - (DateTime)ctx.Continuation.Param).Seconds > 10)
            {
                return Result.Success;
            }

            return Result.Continue;
        }
    }

    public class AttackState : IState<ICleverUnit, Result>
    {
        public Result Drive(Context<ICleverUnit> ctx)
        {
            var rate = ctx.Self.GetFleeBloodRate();
            if (ctx.Self.HpRateLessThan(rate))
            {
                return Result.Success;
            }

            return Result.Continue;
        }
    }

    public class FleeState : IState<ICleverUnit, Result>
    {
        public Result Drive(Context<ICleverUnit> ctx)
        {
            if (ctx.Continuation == null)
            {
                ctx.Self.Flee();
            }

            var unit = ctx.Self.GetNearestTarget();
            if (unit == null)
            {
                return Result.Success;
            }

            if (ctx.Continuation == null)
            {
                ctx.Continuation = new Continuation();
            }

            return Result.Continue;
        }
    }
}
