"""Flax-based ResNet policy-value network for AlphaZero."""

import jax
import jax.numpy as jnp
import flax.linen as nn
from typing import Sequence


class ResidualBlock(nn.Module):
    """A residual block with two conv layers."""
    features: int

    @nn.compact
    def __call__(self, x, train: bool = True):
        residual = x
        x = nn.BatchNorm(use_running_average=not train)(x)
        x = nn.relu(x)
        x = nn.Conv(self.features, kernel_size=(3, 3), padding='SAME')(x)
        x = nn.BatchNorm(use_running_average=not train)(x)
        x = nn.relu(x)
        x = nn.Conv(self.features, kernel_size=(3, 3), padding='SAME')(x)
        return x + residual


class ResNetPolicyValue(nn.Module):
    """ResNet-based policy-value network for AlphaZero.
    
    Two-head network:
                        ┌─> action head (policy logits)
    input ─> Backbone  ─┤
                        └─> value head (scalar value)
    """
    num_actions: int
    num_filters: int = 64
    num_blocks: int = 5

    @nn.compact
    def __call__(self, x, train: bool = True):
        # x shape: (batch, height, width, channels)
        batch_size = x.shape[0]
        
        # Initial convolution
        x = nn.Conv(self.num_filters, kernel_size=(3, 3), padding='SAME')(x)
        x = nn.BatchNorm(use_running_average=not train)(x)
        x = nn.relu(x)
        
        # Residual blocks
        for _ in range(self.num_blocks):
            x = ResidualBlock(self.num_filters)(x, train=train)
        
        # Policy head
        policy = nn.Conv(32, kernel_size=(1, 1))(x)
        policy = nn.BatchNorm(use_running_average=not train)(policy)
        policy = nn.relu(policy)
        policy = policy.reshape((batch_size, -1))  # Flatten
        policy = nn.Dense(self.num_actions)(policy)
        
        # Value head
        value = nn.Conv(32, kernel_size=(1, 1))(x)
        value = nn.BatchNorm(use_running_average=not train)(value)
        value = nn.relu(value)
        value = value.reshape((batch_size, -1))  # Flatten
        value = nn.Dense(64)(value)
        value = nn.relu(value)
        value = nn.Dense(1)(value)
        value = nn.tanh(value)
        
        return policy, value.squeeze(-1)


def create_policy_value_network(num_actions: int, num_filters: int = 64, num_blocks: int = 5):
    """Create a policy-value network."""
    return ResNetPolicyValue(
        num_actions=num_actions,
        num_filters=num_filters,
        num_blocks=num_blocks
    )


def init_network(rng, model, input_shape):
    """Initialize network parameters."""
    dummy_input = jnp.ones((1,) + input_shape)
    variables = model.init(rng, dummy_input, train=False)
    return variables


if __name__ == "__main__":
    # Test the network
    import numpy as np
    
    # T&E dimensions
    H, W, C = 11, 16, 52
    num_actions = 1955
    
    model = create_policy_value_network(num_actions, num_filters=64, num_blocks=5)
    
    rng = jax.random.PRNGKey(0)
    variables = init_network(rng, model, (H, W, C))
    
    # Test forward pass
    dummy_state = jnp.ones((1, H, W, C))
    policy, value = model.apply(variables, dummy_state, train=False)
    
    print(f"Input shape: {dummy_state.shape}")
    print(f"Policy shape: {policy.shape}")
    print(f"Value shape: {value.shape}")
    print(f"Policy sum (should be ~0 before softmax): {policy.sum():.4f}")
    print(f"Value (should be in [-1, 1]): {value[0]:.4f}")
    
    # Count parameters
    param_count = sum(x.size for x in jax.tree_util.tree_leaves(variables['params']))
    print(f"Total parameters: {param_count:,}")
