using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using StateMachine;

namespace HFSM
{
    public enum Result
    {
        Continue,
        Success,
        Failure,
    }
    public enum CleverUnitStateEnum
    {
        Patrol,
        Attack,
        Flee,
        MoveTo,
        Idle,
    }

    public interface IState<TState, TCleverUnit, TResult> 
        where TState : IConvertible
    {
        TState Enum { get; }
        TCleverUnit Self { get; }
        void OnEnter();
        TResult Drive();
        void OnExit();
    }

    public abstract class UnitStateBase : IState<CleverUnitStateEnum, ICleverUnit, Result>
    {
        public CleverUnitStateEnum Enum { get; private set; }
        public ICleverUnit Self { get; private set; }
        protected UnitStateBase(CleverUnitStateEnum state, ICleverUnit self)
        {
            Enum = state;
            Self = self;
        }

        public virtual void OnEnter()
        {
        }
        public abstract Result Drive();
        public virtual void OnExit()
        {
        }
    }

    public abstract class UnitCompositeStateBase : UnitStateBase
    {
        protected readonly LinkedList<UnitStateBase> subStates = new LinkedList<UnitStateBase>();

        protected UnitCompositeStateBase(CleverUnitStateEnum state, ICleverUnit self)
            : base(state, self)
        {
        }

        protected Result ProcessSubStates()
        {
            if (subStates.Count == 0)
            {
                return Result.Success;
            }

            var front = subStates.First;
            var res = front.Value.Drive();

            if (res != Result.Continue)
            {
                subStates.RemoveFirst();
            }

            return Result.Continue;
        }

        protected void AddSubState(UnitStateBase state)
        {
            subStates.AddFirst(state);
        }
    }

    public class PatrolState : UnitCompositeStateBase
    {
        public PatrolState(ICleverUnit self)
            : base(CleverUnitStateEnum.Patrol, self)
        {
        }

        public override void OnEnter()
        {
            base.OnEnter();

            AddSubState(new MoveToState(Self));
        }

        public override Result Drive()
        {
            if (subStates.Count == 0)
            {
                return Result.Success;
            }

            var unit = Self.GetNearestTarget();
            if (unit != null)
            {
                Self.LockTarget(unit);
                return Result.Success;
            }

            var front = subStates.First;
            var ret = front.Value.Drive();

            if (ret != Result.Continue)
            {
                if (front.Value.Enum == CleverUnitStateEnum.MoveTo)
                {
                    AddSubState(new IdleState(Self));
                }
                else
                {
                    AddSubState(new MoveToState(Self));
                }
            }
            
            return Result.Continue;
        }
    }

    public class MoveToState : UnitStateBase
    {
        public MoveToState(ICleverUnit self)
            : base(CleverUnitStateEnum.MoveTo, self)
        {
        }

        public override void OnEnter()
        {
            base.OnEnter();
            Self.MoveToNextPatrolPoiont();
        }

        public override Result Drive()
        {
            if (Self.ReachCurrentPatrolPoint())
            {
                return Result.Success;
            }

            return Result.Continue;
        }
    }

    public class IdleState : UnitStateBase
    {
        private DateTime startTime;
        public IdleState(ICleverUnit self)
            : base(CleverUnitStateEnum.Idle, self)
        {
        }

        public override void OnEnter()
        {
            base.OnEnter();

            Self.Idle();
            startTime = DateTime.Now;
        }

        public override Result Drive()
        {
            if ((DateTime.Now - startTime).Seconds > 10)
            {
                return Result.Success;
            }

            return Result.Continue;
        }
    }

    public class AttackState : UnitStateBase
    {
        public AttackState(ICleverUnit self)
            : base(CleverUnitStateEnum.Attack, self)
        {
        }

        public override Result Drive()
        {
            var rate = Self.GetFleeBloodRate();
            if (Self.HpRateLessThan(rate))
            {
                return Result.Success;
            }

            return Result.Continue;
        }
    }

    public class FleeState : UnitStateBase
    {
        public FleeState(ICleverUnit self)
            : base(CleverUnitStateEnum.Flee, self)
        {
        }
        public override void OnEnter()
        {
            Self.Flee();
        }
        public override Result Drive()
        {
            var unit = Self.GetNearestTarget();
            if (unit != null)
            {
                return Result.Continue;
            }

            return Result.Success;
        }
    }
}
