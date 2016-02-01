using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using StateMachine;

namespace ContextFreeFSM
{
    public interface IState<TState, TUnit> where TState : IConvertible
    {
        TState Enum { get; }
        void OnEnter(TUnit self);
        void Drive(TUnit self);
        void OnExit(TUnit self);
    }

    public abstract class UnitStateBase : IState<UnitStateEnum, IUnit>
    {
        public UnitStateEnum Enum { get; private set; }
        protected UnitStateBase(UnitStateEnum state)
        {
            Enum = state;
        }
        public virtual void OnEnter(IUnit self)
        {
        }
        public virtual void Drive(IUnit self)
        {
        }
        public virtual void OnExit(IUnit self)
        {
        }
    }
    public class PatrolState : UnitStateBase
    {
        public PatrolState() : base(UnitStateEnum.Patrol)
        {
        }

        public override void OnEnter(IUnit self)
        {
            base.OnEnter(self);
            self.Patrol();
        }
        public override void Drive(IUnit self)
        {
            base.Drive(self);
            var unit = self.GetNearestTarget();
            if (unit == null)
            {
                return;
            }

            self.LockTarget(unit);
            self.ChangeState(UnitStateEnum.Attack);
        }
    }

    public class AttackState : UnitStateBase
    {
        public AttackState() : base(UnitStateEnum.Attack)
        {
        }
        public override void Drive(IUnit self)
        {
            base.Drive(self);

            var rate = self.GetFleeBloodRate();
            if (self.HpRateLessThan(rate))
            {
                self.ChangeState(UnitStateEnum.Flee);
            }
        }
    }

    public class FleeState : UnitStateBase
    {
        public FleeState() : base(UnitStateEnum.Flee)
        {
        }
        public override void OnEnter(IUnit self)
        {
            base.OnEnter(self);
            self.Flee();
        }
        public override void Drive(IUnit self)
        {
            base.Drive(self);

            var unit = self.GetNearestTarget();
            if (unit != null)
            {
                return;
            }

            self.ChangeState(UnitStateEnum.Patrol);
        }
    }
}
