export type CheckpointAction = "snapshot" | "restore";
export interface RepositoryContext { repositoryRoot: string; workspaceRoot: string; scopePathspec: string; }
export interface Checkpoint { oid: string; parentOid?: string; timestamp: number; name: string; action: CheckpointAction; restoredFrom?: string; }
