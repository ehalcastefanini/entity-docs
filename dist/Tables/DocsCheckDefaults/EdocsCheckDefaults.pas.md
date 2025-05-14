<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal e Problema Resolvido:**
  O objetivo principal deste código é fornecer uma interface para a manutenção de verificações de documentos (Documents Check Maintenance). Ele permite que os usuários visualizem, adicionem, modifiquem e excluam documentos associados a um registro específico. O código gerencia a interação entre a interface do usuário e os serviços de backend para manipulação de dados.

* **Tecnologias Utilizadas:**
  - Delphi (Object Pascal) para desenvolvimento da aplicação.
  - Componentes visuais como `TsPanel`, `TsPageControl`, `TsTabSheet`, e `TcxGrid` para a interface gráfica.
  - Serviços de backend para manipulação de dados, como `TCheckListDocsDefaultServiceUtils`.

* **Forma do Componente:**
  - **Forma:** Este código implementa um formulário com elementos de entrada e exibição de dados.
    - **Elementos do Formulário:**
      - `TsPanel`: Painéis para organização da interface.
      - `TsPageControl` e `TsTabSheet`: Controle de abas para exibição de informações adicionais.
      - `TcxGrid`: Grade para exibição de documentos.
      - Botões como `BTNadd` e `BTNdelete` para ações específicas.
    - **Ações do Formulário:**
      - Adicionar documentos.
      - Excluir documentos.
      - Imprimir o registro atual.

---

## 2. Descrição da Funcionalidade:

* **Ações Específicas:**
  - Adicionar documentos à lista.
  - Excluir documentos existentes.
  - Imprimir o registro atual.
  - Carregar dados do backend para exibição e manipulação.

* **Componentes Principais:**
  - `FRAMEdocsCheckDefaultsDocs1`: Gerencia a exibição e manipulação de documentos.
  - `FRAMEdocsCheckDefaults`: Gerencia informações adicionais relacionadas aos documentos.
  - `TCheckListDocsDefaultServiceUtils`: Serviço utilizado para manipulação de dados no backend.

* **Tradução para Pseudo-código:**
  - Evento `OnClick` do botão `BTNadd`: `se botão clicado então adicionar documento`.
  - Evento `OnClick` do botão `BTNdelete`: `se botão clicado então excluir documento`.
  - Evento `OnClick` do botão de impressão: `se botão clicado então imprimir registro atual`.
  - Evento de carregamento de dados: `se formulário inicializado então carregar dados do backend`.

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  1. Inicialização do formulário (`m_CreateFormEdit`).
  2. Carregamento de dados do backend (`m_getData`).
  3. Interação do usuário com a interface (adicionar, excluir ou imprimir documentos).
  4. Atualização da interface com base nas ações do usuário.

* **Dados Necessários:**
  - Chaves primárias para identificar registros.
  - Informações de documentos a serem adicionados ou modificados.

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - O botão "Excluir" (`BTNdelete`) só é habilitado se o modo de acesso não for "VIEW" e a lista de documentos não estiver vazia.
  - O botão "Adicionar" (`BTNadd`) está sempre habilitado.

* **Filtros Disponíveis:**
  - Não há filtros explícitos definidos no código.

* **Mensagens de Erro:**
  - Não há mensagens de erro explícitas definidas no código.

* **Valores Padrão dos Campos:**
  - Não há valores padrão explícitos definidos no código.

* **Validações e Condições dos Campos:**
  - Não há validações explícitas definidas no código.

---

## 5. Funções Principais:

* **`m_CreateFormEdit`:**
  - Cria e inicializa o formulário de edição.

* **`m_getData`:**
  - Carrega os dados do backend e configura os parâmetros do serviço.

* **`BTprintCurrentRecordClick`:**
  - Imprime o registro atual.

* **`FRAMEdocsCheckDefaultsDocs1BTNaddClick`:**
  - Adiciona um novo documento à lista.

---

## 6. Consumo de Serviços de API:

* **Serviço Utilizado:** `TCheckListDocsDefaultServiceUtils`.
  - **Endpoint:** Não especificado no código.
  - **Dados Enviados:** Chaves primárias e parâmetros de serviço.
  - **Dados Recebidos:** Dados relacionados aos documentos.
  - **Propósito:** Carregar e manipular dados de documentos.
  - **Tratamento de Erros:** Não especificado no código.

---

## 7. Campos Condicionais (Lógica do Formulário):

* Não há campos condicionais explícitos definidos no código.

---

## 8. Dependências:

* **Bibliotecas Externas:**
  - `kneUtils`, `Global`, `CheckListDocsDefaultServiceUtils`: Utilizadas para manipulação de dados e serviços.

* **Componentes Personalizados:**
  - `TFRAMEdocsCheckDefaultsDocs` e `TFRAMEdocsCheckDefaults`: Componentes personalizados para gerenciamento de documentos.

---

## 9. Listagem de Campos e Validações:

* **Campos:**
  - Documentos (tipo: lista, obrigatório).
  - Chaves primárias (tipo: string, obrigatório).

* **Mapeamento de Valores e Colunas do Banco de Dados:**
  - Não especificado no código.

---

## 10. Exemplos e Diagramas:

* **Fluxograma:** Não aplicável.
* **Diagrama de Sequência:** Não aplicável.
* **Trechos de Código:**
  ```pascal
  procedure TFORMEdocsCheckDefaults.m_getData;
  begin
    // Carrega dados do backend
  end;
  ```
* **Capturas de Tela:** Não aplicável.

---

## 11. Comentários Importantes no Código:

* O método `m_getData` contém lógica para carregar dados do backend e configurar parâmetros de serviço.
* O método `m_CreateFormEdit` é responsável por inicializar o formulário.

---

## 12. Conclusão:

O código fornece uma interface funcional para manutenção de verificações de documentos. Ele é bem estruturado, mas carece de validações explícitas e mensagens de erro. Além disso, a documentação de serviços de backend poderia ser mais detalhada.

---

## 13. Resumo Curto:

Este código implementa um formulário para manutenção de verificações de documentos, permitindo adicionar, excluir e imprimir documentos. Ele interage com serviços de backend para manipulação de dados e utiliza componentes personalizados para gerenciar a interface.#### **EdocsCheckDefaults.pas**

```
unit EdocsCheckDefaults;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneCBedit, StdCtrls, Buttons, ExtCtrls, kneFREditSOA,
  kneFRGridEditSOA, FRdocsCheck, kneFRCtrlEditSOA,
  DBClient, knePrivileges, ImgList, sSpeedButton, sBitBtn, ToolWin,
  ComCtrls, acCoolBar, sPanel, kneEnterAsTab, sPageControl, ActnList,
  sSplitter, FRdocsCheckDefaultsDocs, FRdocsCheckDefaults;

type
  TFORMEdocsCheckDefaults = class(TFORMkneBaseEdit)
    PNLaddInfo: TsPanel;
    PGCdocuments: TsPageControl;
    TSHaddInfo: TsTabSheet;
    FRAMEdocsCheckDefaultsDocs1: TFRAMEdocsCheckDefaultsDocs;
    FRAMEdocsCheckDefaults1: TFRAMEdocsCheckDefaults;
    procedure BTprintCurrentRecordClick(Sender: TObject);
    procedure FRAMEdocsCheckDefaultsDocs1BTNaddClick(Sender: TObject);
  private
    { Private declarations }
  protected
    procedure m_getData; override;
  public
    { Public declarations }
    class function m_CreateFormEdit(const AOwner: TComponent): TFORMkneBaseEdit; override;
  end;

var
  FORMEdocsCheckDefaults: TFORMEdocsCheckDefaults;

implementation

{$R *.dfm}

uses
  kneUtils, Global,
  //---
  CheckListDocsDefaultServiceUtils;

{ TFORMEdocsCheckDefaults }

class function TFORMEdocsCheckDefaults.m_CreateFormEdit(const AOwner: TComponent): TFORMkneBaseEdit;
begin
  // Substituir pelo nome do form
  Result := TFORMEdocsCheckDefaults.Create(Application);
end;

procedure TFORMEdocsCheckDefaults.m_getData;
var
  lv_MasterFrame: TFRAMEBaseEditSOA;
  lv_Keys: TStringList;
begin
  Screen.Cursor := crHourGlass;
  // optimiza��o de recursos
  lv_MasterFrame := TFRAMEBaseEditSOA(kneUtils.TkneGeneric.fg_GetMasterFrame(Self));

  FRAMEdocsCheckDefaultsDocs1.MasterSource := lv_MasterFrame.DStable;
  FRAMEdocsCheckDefaultsDocs1.ProviderService := lv_MasterFrame.ProviderService;


  if mv_keyValues <> '' then
  begin
    try
      lv_Keys := nil;
      lv_Keys := TStringList.Create;
      TkneGeneric.SplitStrings(mv_keyValues, lv_Keys, ';', True);
      with TCheckListDocsDefaultServiceUtils(lv_MasterFrame.ProviderService).Params do
      begin
        consMkt :=  lv_Keys[0];
//        if (lv_Keys[1] <> 'ALL') then
          consignee :=  lv_Keys[1]
//        else
//          consignee :=  '';
      end;
    finally
        if Assigned(lv_Keys) then FreeAndNil(lv_Keys);
    end;
  end;

  // parametros standard de servi�os
  lv_MasterFrame.ServiceParams.ShowInactives := True;
//  lv_MasterFrame.ServiceParams.MaxRecords := 0;
//  lv_MasterFrame.ServiceParams.Criteria := '';

  inherited m_getData;

  if (stringAccessMode = 'NEW') then
  begin
    FRAMEdocsCheckDefaultsDocs1.BTNdelete.Enabled := False;
  end else
  begin
    FRAMEdocsCheckDefaultsDocs1.BTNdelete.Enabled := (stringAccessMode <> 'VIEW')
      and (not FRAMEdocsCheckDefaultsDocs1.CDStable.IsEmpty);
      
    if (stringAccessMode = 'MODIFY') then
      FRAMEdocsCheckDefaultsDocs1.AddedDocs := '|' +                                    //obtem a lista de Docs adicionados
        GetFieldValuesFromCDS(FRAMEdocsCheckDefaultsDocs1.CDStable, 'docCd', '|') + '|';
```

#### **EdocsCheckDefaults.dfm**

```
inherited FORMEdocsCheckDefaults: TFORMEdocsCheckDefaults
  Left = 402
  Top = 125
  Width = 780
  Height = 627
  Caption = 'Documents Check Maintenance'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited PNLtoolbar: TsPanel
    Width = 772
    inherited CLBactions: TsCoolBar
      Width = 772
      Bands = <
        item
          Control = PNbotoes
          ImageIndex = -1
          MinHeight = 41
          Width = 768
        end>
      inherited PNbotoes: TsPanel
        Width = 755
      end
    end
  end
  object PNLdocsCheckDefaults: TsPanel [2]
    Left = 0
    Top = 41
    Width = 772
    Height = 69
    Align = alTop
    TabOrder = 1
    SkinData.SkinSection = 'PANEL'
    inline FRAMEdocsCheckDefaults1: TFRAMEdocsCheckDefaults
      Left = 1
      Top = 1
      Width = 770
      Height = 67
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentBackground = False
      ParentFont = False
      TabOrder = 0
      inherited PNLfooter: TsPanel
        Top = 33
        Width = 770
      end
    end
  end
  object PNLaddInfo: TsPanel [3]
    Left = 0
    Top = 110
    Width = 772
    Height = 490
    Align = alClient
    TabOrder = 2
    SkinData.SkinSection = 'PANEL'
    object PGCdocuments: TsPageControl
      Left = 1
      Top = 1
      Width = 770
      Height = 488
      ActivePage = TSHaddInfo
      Align = alClient
      TabOrder = 0
      SkinData.SkinSection = 'PAGECONTROL'
      object TSHaddInfo: TsTabSheet
        Caption = 'Documents'
        SkinData.CustomColor = False
        SkinData.CustomFont = False
        inline FRAMEdocsCheckDefaultsDocs1: TFRAMEdocsCheckDefaultsDocs
          Left = 0
          Top = 0
          Width = 762
          Height = 460
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentBackground = False
          ParentFont = False
          TabOrder = 0
          inherited cxDBG: TcxGrid
            Width = 762
            Height = 426
          end
          inherited PNLfooter: TsPanel
            Top = 426
            Width = 762
            inherited PNLeditActions: TsPanel
              inherited PNLaddAction: TsPanel
                inherited BTNadd: TsBitBtn
                  OnClick = FRAMEdocsCheckDefaultsDocs1BTNaddClick
                end
```
<!-- tabs:end -->

