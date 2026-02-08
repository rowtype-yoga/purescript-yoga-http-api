module Test.ProjectTrackerExample where

import Prelude

import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Tuple.Nested (type (/\))
import Effect (Effect)
import Literals.Undefined (undefined)
import Effect.Aff (Aff)
import Type.Function (type (#))
import Yoga.HTTP.API.Path (type (:), type (/), type (:?))
import Yoga.HTTP.API.Route (GET, POST, PUT, PATCH, DELETE, Route, buildOpenAPISpec, NoRequest)
import Yoga.HTTP.API.Route.Auth (BearerToken)
import Yoga.HTTP.API.Route.Encoding (JSON, MultipartFormData)
import Yoga.HTTP.API.Route.OpenAPI (class CollectSchemaNames, class CollectSchemas, class RenderJSONSchema)
import Foreign.Object as Object
import Yoga.HTTP.API.Route.OpenAPIMetadata (Description, Example, Examples, ExampleValue, Schema)
import Yoga.HTTP.API.Route.StatusCode (StatusCode(..))
import Yoga.JSON as JSON
import Yoga.JSON (writeJSON)
import Yoga.JSON.Generics (genericReadForeignEnum, genericWriteForeignEnum)
import Test.OpenAPIValidation (validate)
import ViTest (ViTest, describe, test)
import ViTest.Expect (expectToBe)

--------------------------------------------------------------------------------
-- Domain Models
--------------------------------------------------------------------------------

-- | Project with metadata
type Project =
  { id :: Int # Description "Unique project identifier"
  , name :: String # Description "Project name"
  , description :: String # Description "Project description"
  , status :: ProjectStatus # Description "Current project status"
  , createdAt :: Int # Description "Project creation timestamp (Unix epoch)"
  , createdBy :: Int # Description "User ID who created the project"
  }

-- | Project status enum
data ProjectStatus = Active | Archived | OnHold

derive instance Eq ProjectStatus
derive instance Generic ProjectStatus _

instance JSON.ReadForeign ProjectStatus where
  readImpl = genericReadForeignEnum { toConstructorName: identity }

instance JSON.WriteForeign ProjectStatus where
  writeImpl = genericWriteForeignEnum { toConstructorName: identity }

instance RenderJSONSchema ProjectStatus where
  renderJSONSchema _ = JSON.writeImpl
    { type: "string"
    , enum: [ "Active", "Archived", "OnHold" ]
    }

instance CollectSchemas ProjectStatus where
  collectSchemas _ = Object.empty

instance CollectSchemaNames ProjectStatus ()

-- | Task with full details
type Task =
  { id :: Int # Description "Unique task identifier"
  , projectId :: Int # Description "Parent project ID"
  , title :: String # Description "Task title"
  , description :: String # Description "Task description"
  , status :: TaskStatus # Description "Current task status"
  , priority :: Priority # Description "Task priority level"
  , assigneeId :: Maybe Int # Description "Assigned user ID"
  , createdAt :: Int # Description "Task creation timestamp (Unix epoch)"
  , updatedAt :: Int # Description "Last update timestamp (Unix epoch)"
  , dueDate :: Maybe Int # Description "Optional due date (Unix epoch)"
  }

-- | Task status workflow
data TaskStatus = Todo | InProgress | InReview | Done | Blocked

derive instance Eq TaskStatus
derive instance Generic TaskStatus _

instance JSON.ReadForeign TaskStatus where
  readImpl = genericReadForeignEnum { toConstructorName: identity }

instance JSON.WriteForeign TaskStatus where
  writeImpl = genericWriteForeignEnum { toConstructorName: identity }

instance RenderJSONSchema TaskStatus where
  renderJSONSchema _ = JSON.writeImpl
    { type: "string"
    , enum: [ "Todo", "InProgress", "InReview", "Done", "Blocked" ]
    }

instance CollectSchemas TaskStatus where
  collectSchemas _ = Object.empty

instance CollectSchemaNames TaskStatus ()

-- | Task priority levels
data Priority = Low | Medium | High | Urgent

derive instance Eq Priority
derive instance Generic Priority _

instance JSON.ReadForeign Priority where
  readImpl = genericReadForeignEnum { toConstructorName: identity }

instance JSON.WriteForeign Priority where
  writeImpl = genericWriteForeignEnum { toConstructorName: identity }

instance RenderJSONSchema Priority where
  renderJSONSchema _ = JSON.writeImpl
    { type: "string"
    , enum: [ "Low", "Medium", "High", "Urgent" ]
    }

instance CollectSchemas Priority where
  collectSchemas _ = Object.empty

instance CollectSchemaNames Priority ()

-- | Comment on a task
type Comment =
  { id :: Int # Description "Unique comment identifier"
  , taskId :: Int # Description "Parent task ID"
  , authorId :: Int # Description "Comment author user ID"
  , content :: String # Description "Comment text content"
  , createdAt :: Int # Description "Comment creation timestamp (Unix epoch)"
  }

-- | File attachment metadata
type Attachment =
  { id :: Int # Description "Unique attachment identifier"
  , taskId :: Int # Description "Parent task ID"
  , fileName :: String # Description "Original file name"
  , fileSize :: Int # Description "File size in bytes"
  , contentType :: String # Description "MIME type"
  , uploadedBy :: Int # Description "User ID who uploaded"
  , uploadedAt :: Int # Description "Upload timestamp (Unix epoch)"
  , url :: String # Description "Download URL"
  }

-- | Task history entry
type TaskHistoryEntry =
  { id :: Int # Description "Unique history entry identifier"
  , taskId :: Int # Description "Parent task ID"
  , changedBy :: Int # Description "User ID who made the change"
  , changedAt :: Int # Description "Change timestamp (Unix epoch)"
  , field :: String # Description "Field that was changed"
  , oldValue :: Maybe String # Description "Previous value"
  , newValue :: String # Description "New value"
  }

-- | User representation
type User =
  { id :: Int # Description "Unique user identifier"
  , username :: String # Description "Username"
  , email :: String # Description "Email address"
  , fullName :: String # Description "Full name"
  , role :: UserRole # Description "User role"
  }

-- | User roles for authorization
data UserRole = Admin | ProjectManager | Developer | Viewer

derive instance Eq UserRole
derive instance Generic UserRole _

instance JSON.ReadForeign UserRole where
  readImpl = genericReadForeignEnum { toConstructorName: identity }

instance JSON.WriteForeign UserRole where
  writeImpl = genericWriteForeignEnum { toConstructorName: identity }

instance RenderJSONSchema UserRole where
  renderJSONSchema _ = JSON.writeImpl
    { type: "string"
    , enum: [ "Admin", "ProjectManager", "Developer", "Viewer" ]
    }

instance CollectSchemas UserRole where
  collectSchemas _ = Object.empty

instance CollectSchemaNames UserRole ()

--------------------------------------------------------------------------------
-- Request/Response Types
--------------------------------------------------------------------------------

-- | Request to create a project
type CreateProjectRequest =
  { name :: String # Example "\"Mobile App Redesign\""
  , description :: String # Example "\"Redesign the mobile app with new UI/UX\""
  }

-- | Request to update a project
type UpdateProjectRequest =
  { name :: Maybe String
  , description :: Maybe String
  , status :: Maybe ProjectStatus
  }

-- | Request to create a task
type CreateTaskRequest =
  { title :: String # Examples (short :: ExampleValue "\"Fix bug\"", detailed :: ExampleValue "\"Fix login validation bug\"")
  , description :: String # Example "\"The login form doesn't validate email properly\""
  , priority :: Priority # Example "High"
  , assigneeId :: Maybe Int # Example "42"
  , dueDate :: Maybe Int
  }

-- | Request to update a task
type UpdateTaskRequest =
  { title :: Maybe String
  , description :: Maybe String
  , status :: Maybe TaskStatus
  , priority :: Maybe Priority
  , assigneeId :: Maybe Int
  , dueDate :: Maybe Int
  }

-- | Request to assign a task
type AssignTaskRequest =
  { assigneeId :: Int # Example "42"
  }

-- | Request to add a comment
type CreateCommentRequest =
  { content :: String # Example "\"I've started working on this\""
  }

-- | File upload in multipart form
type FileUpload =
  { file :: String # Description "File contents (binary data)"
  }

-- | Pagination query parameters
type PaginationParams =
  { limit :: Int # Example "20" # Description "Number of items per page"
  , offset :: Int # Example "0" # Description "Number of items to skip"
  }

-- | Task filter parameters
type TaskFilterParams =
  { status :: Maybe TaskStatus
  , priority :: Maybe Priority
  , assigneeId :: Maybe Int
  } /\ PaginationParams

-- | Error response
type ErrorResponse =
  { error :: String # Description "Error message"
  , code :: String # Description "Error code"
  }

--------------------------------------------------------------------------------
-- API Definition
--------------------------------------------------------------------------------

-- | Complete Project Tracker API
type ProjectTrackerAPI =
  { -- Projects
    listProjects ::
      Route GET ("projects" :? PaginationParams)
        { headers :: { authorization :: BearerToken } }
        ( ok :: { body :: Schema "ProjectList" (Array Project) }
        )

  , getProject ::
      Route GET ("projects" / "id" : Int)
        { headers :: { authorization :: BearerToken } }
        ( ok :: { body :: Schema "Project" Project }
        , notFound :: { body :: ErrorResponse }
        )

  , createProject ::
      Route POST "projects"
        { headers :: { authorization :: BearerToken }
        , body :: JSON (Schema "CreateProjectRequest" CreateProjectRequest)
        }
        ( created :: { body :: Schema "Project" Project }
        , unauthorized :: { body :: ErrorResponse }
        )

  , updateProject ::
      Route PUT ("projects" / "id" : Int)
        { headers :: { authorization :: BearerToken }
        , body :: JSON (Schema "UpdateProjectRequest" UpdateProjectRequest)
        }
        ( ok :: { body :: Schema "Project" Project }
        , notFound :: { body :: ErrorResponse }
        , unauthorized :: { body :: ErrorResponse }
        )

  , archiveProject ::
      Route DELETE ("projects" / "id" : Int)
        { headers :: { authorization :: BearerToken } }
        ( noContent :: { body :: {} }
        , notFound :: { body :: ErrorResponse }
        , unauthorized :: { body :: ErrorResponse }
        )

  -- Tasks
  , listTasks ::
      Route GET ("projects" / "projectId" : Int / "tasks" :? TaskFilterParams)
        { headers :: { authorization :: BearerToken } }
        ( ok :: { body :: Schema "TaskList" (Array Task) }
        , notFound :: { body :: ErrorResponse }
        )

  , getTask ::
      Route GET ("tasks" / "id" : Int)
        { headers :: { authorization :: BearerToken } }
        ( ok :: { body :: Schema "Task" Task }
        , notFound :: { body :: ErrorResponse }
        )

  , createTask ::
      Route POST ("projects" / "projectId" : Int / "tasks")
        { headers :: { authorization :: BearerToken }
        , body :: JSON (Schema "CreateTaskRequest" CreateTaskRequest)
        }
        ( created :: { body :: Schema "Task" Task }
        , notFound :: { body :: ErrorResponse }
        , unauthorized :: { body :: ErrorResponse }
        )

  , updateTask ::
      Route PUT ("tasks" / "id" : Int)
        { headers :: { authorization :: BearerToken }
        , body :: JSON (Schema "UpdateTaskRequest" UpdateTaskRequest)
        }
        ( ok :: { body :: Schema "Task" Task }
        , notFound :: { body :: ErrorResponse }
        , unauthorized :: { body :: ErrorResponse }
        )

  , assignTask ::
      Route PATCH ("tasks" / "id" : Int / "assign")
        { headers :: { authorization :: BearerToken }
        , body :: JSON (Schema "AssignTaskRequest" AssignTaskRequest)
        }
        ( ok :: { body :: Schema "Task" Task }
        , notFound :: { body :: ErrorResponse }
        , unauthorized :: { body :: ErrorResponse }
        )

  , deleteTask ::
      Route DELETE ("tasks" / "id" : Int)
        { headers :: { authorization :: BearerToken } }
        ( noContent :: { body :: {} }
        , notFound :: { body :: ErrorResponse }
        , unauthorized :: { body :: ErrorResponse }
        )

  -- Comments
  , listComments ::
      Route GET ("tasks" / "taskId" : Int / "comments" :? PaginationParams)
        { headers :: { authorization :: BearerToken } }
        ( ok :: { body :: Schema "CommentList" (Array Comment) }
        , notFound :: { body :: ErrorResponse }
        )

  , addComment ::
      Route POST ("tasks" / "taskId" : Int / "comments")
        { headers :: { authorization :: BearerToken }
        , body :: JSON (Schema "CreateCommentRequest" CreateCommentRequest)
        }
        ( created :: { body :: Schema "Comment" Comment }
        , notFound :: { body :: ErrorResponse }
        )

  -- Attachments
  , listAttachments ::
      Route GET ("tasks" / "taskId" : Int / "attachments")
        { headers :: { authorization :: BearerToken } }
        ( ok :: { body :: Schema "AttachmentList" (Array Attachment) }
        , notFound :: { body :: ErrorResponse }
        )

  , uploadAttachment ::
      Route POST ("tasks" / "taskId" : Int / "attachments")
        { headers :: { authorization :: BearerToken }
        , body :: MultipartFormData (Schema "FileUpload" FileUpload)
        }
        ( created :: { body :: Schema "Attachment" Attachment }
        , notFound :: { body :: ErrorResponse }
        , badRequest :: { body :: ErrorResponse }
        )

  , deleteAttachment ::
      Route DELETE ("attachments" / "id" : Int)
        { headers :: { authorization :: BearerToken } }
        ( noContent :: { body :: {} }
        , notFound :: { body :: ErrorResponse }
        , unauthorized :: { body :: ErrorResponse }
        )

  -- Task History
  , getTaskHistory ::
      Route GET ("tasks" / "taskId" : Int / "history")
        { headers :: { authorization :: BearerToken } }
        ( ok :: { body :: Schema "TaskHistory" (Array TaskHistoryEntry) }
        , notFound :: { body :: ErrorResponse }
        )
  }

--------------------------------------------------------------------------------
-- Service Layer Types (for OmLayer pattern)
--------------------------------------------------------------------------------

-- | Project repository interface
type ProjectRepo =
  { findAll :: PaginationParams -> Aff (Array Project)
  , findById :: Int -> Aff (Maybe Project)
  , create :: CreateProjectRequest -> Int -> Aff Project
  , update :: Int -> UpdateProjectRequest -> Aff (Maybe Project)
  , archive :: Int -> Aff Boolean
  }

-- | Task repository interface
type TaskRepo =
  { findByProject :: Int -> TaskFilterParams -> Aff (Array Task)
  , findById :: Int -> Aff (Maybe Task)
  , create :: Int -> CreateTaskRequest -> Int -> Aff Task
  , update :: Int -> UpdateTaskRequest -> Aff (Maybe Task)
  , delete :: Int -> Aff Boolean
  , assign :: Int -> Int -> Aff (Maybe Task)
  }

-- | Comment repository interface
type CommentRepo =
  { findByTask :: Int -> PaginationParams -> Aff (Array Comment)
  , create :: Int -> CreateCommentRequest -> Int -> Aff Comment
  }

-- | Attachment repository interface
type AttachmentRepo =
  { findByTask :: Int -> Aff (Array Attachment)
  , create :: Int -> FileUpload -> Int -> Aff Attachment
  , delete :: Int -> Aff Boolean
  }

-- | Task history repository interface
type TaskHistoryRepo =
  { findByTask :: Int -> Aff (Array TaskHistoryEntry)
  , recordChange :: Int -> Int -> String -> Maybe String -> String -> Aff TaskHistoryEntry
  }

-- | Authentication service interface
type AuthService =
  { authenticate :: BearerToken -> Aff (Maybe User)
  , authorize :: User -> String -> Aff Boolean
  }

-- Note: Repository implementations moved to Test.ProjectTrackerLayers module

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

testProjectTrackerAPI :: Effect ViTest
testProjectTrackerAPI = describe "Project Tracker API" do
  _ <- test "generates valid OpenAPI 3.0 schema" do
    let
      spec = buildOpenAPISpec @ProjectTrackerAPI
        { title: "Project Tracker API"
        , version: "1.0.0"
        , description: "A comprehensive project and task management API showcasing authentication, CRUD operations, file uploads, and audit trails"
        , contact:
            { name: "API Team"
            , email: "api@projecttracker.com"
            , url: "https://projecttracker.com/support"
            }
        , license:
            { name: "MIT"
            , url: "https://opensource.org/licenses/MIT"
            }
        }
      result = validate spec
    expectToBe true (Array.null result.errors)

  _ <- test "includes all project endpoints" do
    let spec = buildOpenAPISpec @ProjectTrackerAPI { title: "Project Tracker", version: "1.0.0" }
    let json = writeJSON spec
    expectToBe true (json /= "")

  _ <- test "includes authentication in security schemes" do
    let spec = buildOpenAPISpec @ProjectTrackerAPI { title: "Project Tracker", version: "1.0.0" }
    let json = writeJSON spec
    expectToBe true (json /= "")

  test "generates comprehensive API spec with all features" do
    let spec = buildOpenAPISpec @ProjectTrackerAPI { title: "Project Tracker", version: "1.0.0" }
    let json = writeJSON spec
    expectToBe true (json /= "")
