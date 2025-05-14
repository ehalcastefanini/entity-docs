<!-- tabs:start -->

#### **Documentation**

# Documentation for `MaddressAndContact` Code Unit

## 1. Overview:

### Objective:
The `MaddressAndContact` code unit is designed to manage and edit addresses and contact information within a user interface. It provides a form-based interface for users to view, edit, and manage address and contact data. The form integrates two main components: an address management frame and a contact management frame, allowing seamless interaction between the two.

### Technologies Used:
- **Delphi VCL Framework**: Used for creating the graphical user interface.
- **TClientDataSet**: Used for managing datasets for addresses and contacts.
- **Third-party Components**: Includes `TsPanel`, `TsSplitter`, `TFRAMEentityAddress`, and `TFRAMEaddressContact` for enhanced UI and functionality.

### Form Type:
This is a **form** with the following elements:
- **Form Elements**:
  - `PNLaddress` (Panel): Contains the address management frame.
  - `FRAMEentityAddress1` (Frame): Manages address-related data.
  - `FRAMEaddressContact1` (Frame): Manages contact-related data.
  - `SPLseparator` (Splitter): Separates the address and contact sections.
  - `BTApply` (Button): Applies changes.
  - `BTCancel` (Button): Cancels changes.
- **Form Actions**:
  - `BTApplyClick`: Saves changes to the address and contact data.
  - `BTCancelClick`: Cancels any unsaved changes.

---

## 2. Functionality Description:

### User/Software Actions:
- Users can view, edit, and manage address and contact data.
- Users can apply or cancel changes using the provided buttons.

### Main Components:
1. **`FRAMEentityAddress1`**: Handles address-related data.
2. **`FRAMEaddressContact1`**: Handles contact-related data.
3. **`SPLseparator`**: Provides a visual and functional separation between address and contact sections.

### Pseudo-code for Actions and Events:
- **`BTApplyClick` Event**:
  ```pseudo
  if Apply button clicked then
      save address and contact data
  ```
- **`BTCancelClick` Event**:
  ```pseudo
  if Cancel button clicked then
      discard unsaved changes
  ```
- **`m_getData` Method**:
  ```pseudo
  if form initialized then
      setup master-detail relationships
      collect data from master frame
      initialize save points for address and contact
  ```

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The form is created using `m_CreateFormEdit`.
   - The `m_getData` method is called to set up data relationships and initialize save points.
2. **User Interaction**:
   - Users interact with the address and contact frames to edit data.
   - Users can apply or cancel changes using the respective buttons.
3. **Functions**:
   - `m_CreateFormEdit` (File: `MaddressAndContact`): Creates the form.
   - `m_getData` (File: `MaddressAndContact`): Sets up data relationships and initializes save points.
   - `BTApplyClick` (File: `MaddressAndContact`): Saves changes.
   - `BTCancelClick` (File: `MaddressAndContact`): Cancels changes.

### Required Data:
- Address and contact datasets must be provided to the form for it to function correctly.

---

## 4. Business Rules:

### Actions and Preconditions:
- **Apply Button**:
  - Action: Saves changes.
  - Preconditions: Address and contact data must be valid.
- **Cancel Button**:
  - Action: Discards changes.
  - Preconditions: None.

### Filters:
- No explicit filters are defined in the code.

### Error Messages:
- No explicit error messages are defined in the code.

### Default Field Values:
- Default values are not explicitly defined in the code.

### Field Validation and Conditions:
- Field validations are not explicitly defined in the code.

---

## 5. Main Functions:

1. **`m_CreateFormEdit`**:
   - Creates and initializes the form.
2. **`m_getData`**:
   - Sets up master-detail relationships and initializes save points.
3. **`BTApplyClick`**:
   - Saves changes to the address and contact data.
4. **`BTCancelClick`**:
   - Cancels any unsaved changes.

---

## 6. API Service Consumption:

- No external API services are consumed in this code.

---

## 7. Conditional Fields (Form Logic):

- No conditional fields are explicitly defined in the code.

---

## 8. Dependencies:

### External Libraries:
- **`TsPanel`, `TsSplitter`, `TFRAMEentityAddress`, `TFRAMEaddressContact`**: Used for UI components and functionality.

### Custom Components:
- **`TFRAMEentityAddress`**: Manages address data.
- **`TFRAMEaddressContact`**: Manages contact data.

---

## 9. Fields and Validations Listing:

### Fields:
- **Address Fields**: Managed by `FRAMEentityAddress1`.
- **Contact Fields**: Managed by `FRAMEaddressContact1`.

### Mapping:
- Field mappings to database columns are not explicitly defined in the code.

---

## 10. Examples and Diagrams:

### Flowchart:
```plaintext
[Start] --> [Initialize Form] --> [Load Data] --> [User Interaction] --> [Apply or Cancel Changes] --> [End]
```

### Sequence Diagram:
```plaintext
User --> Form: Open Form
Form --> Dataset: Load Data
User --> Form: Edit Data
User --> Form: Click Apply/Cancel
Form --> Dataset: Save/Discard Changes
```

### Code Snippets:
```delphi
procedure TFORMMaddressAndContact.BTApplyClick(Sender: TObject);
begin
  // Save changes to address and contact data
end;
```

### Screenshots:
Not applicable (no DFM file provided).

---

## 11. Important Comments in the Code:

- **`m_getData`**: Sets up master-detail relationships and initializes save points.
- **`m_showModal`**: Overridden to prevent execution until datasets are provided.

---

## 12. Conclusion:

The `MaddressAndContact` code unit provides a robust interface for managing address and contact data. Its modular design, with separate frames for address and contact management, ensures flexibility and maintainability. However, the lack of explicit error handling and field validations may limit its robustness in certain scenarios.

---

## 13. Short Summary:

The `MaddressAndContact` unit is a Delphi-based form for managing address and contact data, featuring modular frames and master-detail relationships. It supports data editing and saving but lacks explicit error handling and field validations.#### **MaddressAndContact.pas**

```
unit MaddressAndContact;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneCBedit, StdCtrls, Buttons, ExtCtrls, kneFREditSOA,
  kneFRGridEditSOA, FREntityAddress, FRaddressContact,
  knePrivileges, ImgList, sSpeedButton, sBitBtn, ToolWin, ComCtrls,
  acCoolBar, sPanel, kneEnterAsTab, ActnList, DBClient, kneFRCtrlEditSOA,
  kneTypes, sSplitter;

type
  TFORMMaddressAndContact = class(TFORMkneBaseEdit)
    PNLaddress: TsPanel;
    FRAMEentityAddress1: TFRAMEentityAddress;
    FRAMEaddressContact1: TFRAMEaddressContact;
    SPLseparator: TsSplitter;
    procedure BTApplyClick(Sender: TObject);
    procedure BTCancelClick(Sender: TObject);
  private
    { Private declarations }
    FAddressSavePoint, FContactSavePoint: Integer;
    FHasPortalChannel: Boolean;
    procedure SetHasPortalChannel(const Value: Boolean);
  protected
    procedure m_getData; override;

  published
    property HasPortalChannel {#24241}: Boolean read FHasPortalChannel write SetHasPortalChannel;

    procedure SetDataSetsReference(pv_ds1, pv_ds2: TClientDataSet;
       pv_AddressSavePoint: Integer);  // recebe as referencias para os dataset dos addresses e Contacts

  public
    { Public declarations }
    class function m_CreateFormEdit(const AOwner: TComponent): TFORMkneBaseEdit; override;
    procedure m_showModal; override;
    procedure RunForm;
  end;

var
  FORMMaddressAndContact: TFORMMaddressAndContact;

implementation

uses
  kneUtils, db;

{$R *.dfm}

{ TFORMMaddressAndContact }

class function TFORMMaddressAndContact.m_CreateFormEdit(const AOwner: TComponent): TFORMkneBaseEdit;
begin
  // Cria��o de FORM EDIT
  Result := TFORMMaddressAndContact.Create(AOwner); // na derivada TFORMExxx.Create(Application);
end;

procedure TFORMMaddressAndContact.m_getData;
var
  lv_MasterFrame: TFRAMEBaseEditSOA;
  lv_Params: TStringList;
  lv_i: Integer;
begin
//  inherited ;
  // Este FORM utiliza os dataset das Frames: FRlistAdresses e FRlistContacts
  // por isso n�o necessita de invocar servi�os, para carregar dados
  try try
    // NOVO:
    // optimiza��o de recursos
    lv_MasterFrame := TFRAMEBaseEditSOA(kneUtils.TkneGeneric.fg_GetMasterFrame(Self));

    // setup das rela��es master-detail
    FRAMEaddressContact1.MasterSource := lv_MasterFrame.DStable;

    m_CollectData(lv_MasterFrame);

    lv_MasterFrame.AccessMode := StringAccessMode;

    FAddressSavePoint := 0;
    FContactSavePoint := 0;
  except
    on e: Exception do //ShowMessage(e.Message);
      raise;
  end; //except
  finally
    if Assigned(lv_MasterFrame) then lv_MasterFrame := nil;
  end; // finally
end;

procedure TFORMMaddressAndContact.m_showModal;
begin
  // � efectuado o verride deste procedimento para que o form n�o seja executado
  // nesta fase. S� deve ser executado apo�s ele receber os endere�os dos dataset
  // para executar o form utilizo o RunForm
end;

procedure TFORMMaddressAndContact.RunForm;
begin
```

#### **MaddressAndContact.dfm**

```
inherited FORMMaddressAndContact: TFORMMaddressAndContact
  Left = 345
  Top = 51
  Width = 913
  Height = 621
  Caption = 'Addresses and Contacts Management'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object SPLseparator: TsSplitter [1]
    Left = 0
    Top = 249
    Width = 905
    Height = 4
    Cursor = crVSplit
    Align = alTop
    SkinData.SkinSection = 'FORM'
  end
  inherited PNLtoolbar: TsPanel
    Width = 905
    inherited CLBactions: TsCoolBar
      Width = 905
      Bands = <
        item
          Control = PNbotoes
          ImageIndex = -1
          MinHeight = 41
          Width = 901
        end>
      inherited PNbotoes: TsPanel
        Width = 888
      end
    end
  end
  object PNLaddress: TsPanel [3]
    Left = 0
    Top = 41
    Width = 905
    Height = 208
    Align = alTop
    TabOrder = 1
    SkinData.SkinSection = 'PANEL'
    inline FRAMEentityAddress1: TFRAMEentityAddress
      Left = 1
      Top = 1
      Width = 903
      Height = 206
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentBackground = False
      ParentFont = False
      TabOrder = 0
      inherited LBLCounty: TsLabel
        FocusControl = FRAMEentityAddress1.FRAMEfindCountry.DBE
      end
      inherited PNLfooter: TsPanel
        Top = 201
        Width = 887
      end
      inherited FRAMEstatusInfo1: TFRAMEstatusInfo
        Top = 159
        Width = 887
        inherited GRPstatus: TsGroupBox
          Width = 887
          inherited ICBOstat: TcxDBImageComboBox
            Width = 97
          end
        end
      end
    end
  end
  inline FRAMEaddressContact1: TFRAMEaddressContact [4]
    Left = 0
    Top = 253
    Width = 905
    Height = 341
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentBackground = False
    ParentFont = False
    TabOrder = 2
    inherited cxDBG: TcxGrid
      Width = 905
      Height = 307
    end
    inherited PNLfooter: TsPanel
      Top = 307
      Width = 905
    end
  end
  inherited PRVprivileges: TPrivileges [5]
  end
```
<!-- tabs:end -->

