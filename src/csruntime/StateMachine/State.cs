using System;
using System.Collections.Generic;
using System.Linq;
using System.Net;
using System.Text;

namespace StateMachine
{
    public interface IState<TState, TUnit> where TState : IConvertible
    {
        TState Enum { get; }
        TUnit Self { get; }
        void OnEnter();
        void Drive();
        void OnExit();
    }

    public enum UnitStateEnum
    {
        Patrol,
        Attack,
        Flee,
    }

    public abstract class UnitStateBase : IState<UnitStateEnum, IUnit>
    {
        public UnitStateEnum Enum { get; private set; }
        public IUnit Self { get; private set; }

        protected UnitStateBase(UnitStateEnum state, IUnit self)
        {
            Enum = state;
            Self = self;
        }
        public virtual void OnEnter()
        {
        }
        public virtual void Drive()
        {
        }
        public virtual void OnExit()
        {
        }
    }
    public class PatrolState : UnitStateBase
    {
        public PatrolState(IUnit self) : base(UnitStateEnum.Patrol, self)
        {
        }

        public override void OnEnter()
        {
            base.OnEnter();
            Self.Patrol();
        }

        public override void Drive()
        {
            base.Drive();
            var unit = Self.GetNearestTarget();
            if (unit == null)
            {
                return;
            }

            Self.LockTarget(unit);
            Self.ChangeState(UnitStateEnum.Attack);
        }
    }

    public class AttackState : UnitStateBase
    {
        public AttackState(IUnit self) : base(UnitStateEnum.Attack, self)
        {
        }
        public override void Drive()
        {
            var rate = Self.GetFleeBloodRate();
            if (Self.HpRateLessThan(rate))
            {
                Self.ChangeState(UnitStateEnum.Flee);
            }
        }
    }

    public class FleeState : UnitStateBase
    {
        public FleeState(IUnit self) : base(UnitStateEnum.Flee, self)
        {
        }
        public override void OnEnter()
        {
            Self.Flee();
        }
        public override void Drive()
        {
            var unit = Self.GetNearestTarget();
            if (unit != null)
            {
                return;
            }

            Self.ChangeState(UnitStateEnum.Patrol);
        }
    }
}
